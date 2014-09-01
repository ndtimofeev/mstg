-- base
import Control.Monad
import Control.Category hiding ((.), id)

import Data.List

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe

-- cpphs
import Language.Preprocessor.Cpphs

-- hslua
import Scripting.Lua hiding ( concat )

-- parsec
import Text.Parsec
import Text.Parsec.String

-- language-lua
import Language.Lua.Syntax
import Language.Lua.Parser
import Language.Lua.PrettyPrinter

-- internal
import Language.Stg.AST
import Language.Stg.Generate.Lua
import Language.Stg.Parser.Simple
import Language.Stg.PrettyPrinter.Naive

data ArgumentStack = GlobalStack | LocalStack
    deriving Show

data OutputType = LuaCode {- [GeneratorConfig] -} | Effect | StgCode [StgPrinterConf]
    deriving Show

data AppOptions = AppOptions
    { output               :: Handle
    , outputType           :: OutputType
    , entryThunk           :: String
    , preprocessorTable    :: [(String, String)]
    , stgToStgPipe         :: [Binding] -> [Binding]
    , stgNormalizationPipe :: [Binding] -> [Binding]
    }

defaultOption :: AppOptions
defaultOption = AppOptions
    { output = stdout
    , outputType = LuaCode
    , entryThunk = "main"
    , stgToStgPipe =
            cutAltsTransform >>>            -- Cut inaccessible case alternative
            singleCaseTransform >>>         -- Simplify case nodes
            deriveFreeVars [] >>>           -- Derive free vars for existing bindings
            deadTopBindingElimination >>>   -- Eliminate toplevel dead bindings
            reverse                         -- Reverse bindings list
    , stgNormalizationPipe = cutAltsTransform >>> singleCaseTransform
    , preprocessorTable = [("BACKPORT", "1"), ("SIMPLEALLOC", "1")]
    }

options :: [OptDescr (AppOptions -> IO AppOptions)]
options =
    [ Option "h" ["help"]
            (NoArg (\_ -> putStrLn appUsageInfo >> exitSuccess))
            "show this help message"

    , Option "o" ["output"]
            (ReqArg (\file opts -> do
                h <- openFile file WriteMode
                return (opts { output = h })) "FILE")
            "write compiler output to FILE"

    , Option "e" ["eval"]
            (NoArg $ \opts -> return $ opts { outputType = Effect })
            "eval program immediate"

    , Option "" ["print-stg"]
            (NoArg $ \opts -> return $ opts { outputType = StgCode [] })
            "only print stg code after transform"

    , Option "" ["print-stg-with-free-vars"]
            (NoArg $ \opts -> return $ opts { outputType = StgCode [ShowFreeVars] })
            "print stg code after transform with free vars annotation"

    , Option "m" ["main"]
            (ReqArg (\name opts -> return $ opts { entryThunk = name }) "THUNK")
            "start program from THUNK"

    , Option "T" ["trace"]
            (NoArg $ \opts -> return $ opts { preprocessorTable = ("OTRACE", "1") : preprocessorTable opts })
            "trace entered objects"

    , Option "S" ["statistics"]
            (NoArg $ \opts -> return $ opts { preprocessorTable = ("RTSINFO", "1") : preprocessorTable opts })
            "keep end print runtime information about program"
    -- , Option "" ["enable-array-cache"]
    --     (OptArg (\mv opts -> do
    --         b <- yesNoRead "enable-array-cache" mv
    --         return (opts { enableMemCache = b })) "yes/no" ) "enable lua array caching"
    -- , Option "" ["enable-global-argstack"]
    --     (OptArg (\mv opts -> return (opts { argumentStackLayout = maybe GlobalStack (\v -> if v == "yes" then GlobalStack else LocalStack) mv })) "yes/no" ) "enable global arguments stack"
    -- , Option "" ["inline-primops"] (OptArg (\mv opts -> do
    --     b <- yesNoRead "inline-primops" mv
    --     return (opts { luaGeneratorConf = enableDisable b (luaGeneratorConf opts) InlinePrimOps })) "yes/no") "enable primitive operation inlining"
    ]
    where
        enableDisable :: Eq a => Bool -> [a] -> a -> [a]
        enableDisable b xs x
            | b, x `notElem` xs = x:xs
            | not b     = delete x xs
            | otherwise = xs

        yesNoRead :: String -> Maybe String -> IO Bool
        yesNoRead optName mstr = case mstr of
            Nothing    -> return True
            Just "yes" -> return True
            Just "no"  -> return False
            Just str   -> do
                putStrLn (str ++ " bad value for " ++ optName)
                putStrLn appUsageInfo
                exitSuccess

appUsageInfo :: String
appUsageInfo = usageInfo ("Usage: " ++ appName ++ " [options] [STGFILES]\n") options
    where
        appName = unsafePerformIO $ getProgName

compilerOpts :: [String] -> IO (AppOptions, [String])
compilerOpts argv = case getOpt' RequireOrder options argv of
    (trans, argv', [], []) -> do
        opt <- foldM (\opt trns -> trns opt) defaultOption trans
        return (opt, argv')
    (_, _, unrecs, errs)   -> do
        forM_ unrecs $ \o -> putStrLn $ "unrecognized option: " ++ o
        forM_ errs $ \e -> putStrLn e
        putStrLn appUsageInfo
        exitFailure

compilerLuaCode :: String -> [Binding] -> AppOptions -> String
compilerLuaCode rts bindings opts = intercalate "\n\n"
    [ rts
    , concatMap
        (\b -> (displayS (renderPretty 1 100 (pprint b)) "\n\n"))
            (generate (stgToStgPipe opts bindings) [] [] [])
    , "ENTER( " ++ entryThunk opts ++ " )" ]

main :: IO ()
main = do
    (opt,argv)  <- getArgs >>= compilerOpts
    rts         <- readFile "lua/rts.lua"
    prim        <- readFile "lua/primops.lua"
    bs          <- case argv of
        []    -> putStrLn appUsageInfo >> exitFailure

        ["-"] -> do
            txt <- getContents
            case parse programStg "" txt of
                Left err -> error (show err)
                Right bs -> return bs

        _     -> liftM concat $ forM argv $ \path -> do
            res <- parseFromFile programStg path
            case res of
                Left err -> error (show err)
                Right bs -> return bs
    rts' <- runCpphs (defaultCpphsOptions { defines = preprocessorTable opt, boolopts = defaultBoolOptions { locations = False } }) "" rts
    let outCode = compilerLuaCode (intercalate "\n\n" [rts', prim]) bs opt
    case outputType opt of
        LuaCode      -> hPutStrLn (output opt) outCode
        Effect       -> do
            l <- newstate
            openlibs l
            loadstring l outCode ""
            call l 0 0
            close l
        StgCode cfgs -> hPutStrLn (output opt) $ pprintStg cfgs $ stgToStgPipe opt bs
    hClose (output opt)
