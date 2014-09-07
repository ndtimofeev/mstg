-- Cabal
import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.BuildPaths

import Distribution.Verbosity

main = defaultMainWithHooks simpleUserHooks
    {
        postConf = \a c p l -> do
            postConf simpleUserHooks a c p l
            createDirectoryIfMissingVerbose silent True (autogenModulesDir l ++ "/Language/Stg/Generate/Lua")
            generateBuitinRTS (autogenModulesDir l)
    }

generateBuitinRTS path = do
    putStrLn ("Generate " ++ path ++ "/Language/Stg/Generate/Lua/RTS.hs...")
    rtsStr  <- readFile "lua/rts.lua"
    primStr <- readFile "lua/primops.lua"
    writeFile (path ++ "/Language/Stg/Generate/Lua/RTS.hs") $ unlines $
        [ "module Language.Stg.Generate.Lua.RTS where"
        , ""
        , "rts :: String"
        , "rts = " ++ show rtsStr
        , ""
        , "primops :: String"
        , "primops = " ++ show primStr
        ]
