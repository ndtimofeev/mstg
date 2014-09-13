module Language.Stg.Generate.Lua where

-- base
import Prelude hiding ( EQ )

import Data.Char
import Data.List

-- mtl
import Control.Monad.Reader
import Control.Monad.State.Strict

-- language-lua
import Language.Lua.Syntax hiding ( GT, LT )

-- syb
import Data.Generics.Aliases
import Data.Generics.Schemes

-- internal
import Language.Stg.AST
import Language.Lua.Build

data GeneratorConfig
    = NativeStack
    | HeapStack
    | UglyGen
    | PureGen
    | InlinePrimOps
    | InlineRTS
    deriving (Show, Eq, Ord)

data GeneratorEnv = GeneratorEnv
    { topLevelBindings :: [String]
    , generatorOpts    :: [GeneratorConfig]
    , primOps          :: [(String, FunBody)]
    , constrPacks      :: [TypeR]
    , nameGenerator    :: [String] -> Maybe String -> String
    , nameObfuscator   :: [String] -> String -> Maybe String -> String
    , caseExprGen      :: StgExpr -> Alts StgExpr -> Generate Block }

data GeneratorState = GeneratorState
    { nameStack        :: [[(Label, String)]]
    , currentNameSpace :: [(Label, String)]
    , constrVariables  :: [(String, String)] }
    deriving Show

type Generate a = StateT GeneratorState (Reader GeneratorEnv) a

luaNameObfuscator :: [String] -> String -> Maybe String -> String
luaNameObfuscator namespace stgName mprefix =
        collisionSolve $ apostropheSanitize $ prefix stgName
    where
        apostropheSanitize :: String -> String
        apostropheSanitize str = flip concatMap str $ \c -> case c of
            '\'' -> "_"
            _    -> [c]

        prefix :: String -> String
        prefix str
            | Just p <- mprefix = p ++ "_" ++ str
            | otherwise         = str

        collisionSolve :: String -> String
        collisionSolve str
            | str `notElem` namespace  = str
            | c <- last str, isDigit c = collisionSolve $ init str ++ show (digitToInt c + 1)
            | '_' <- last str          = collisionSolve $ str ++ "0"
            | otherwise                = collisionSolve $ str ++ "_0"

idBlackList :: [String]
idBlackList =
    -- lua syntax
    [ "nil", "true", "false"
    , "DEFAULT" -- key for default case
    , "i" -- local iterator
    ]

popValues :: String -> [Var] -> [Stat]
popValues arr exps = case exps of
    []      -> []
    (e:es)  -> "i" `lassign` Unop Len (var arr) :
            e `assign` (tableVal arr (var "i") :: Exp) :
            (tableVal arr (var "i") :: Var) `assign` Nil :
            concatMap (\e' -> "i" `assign` (var "i" - 1) : e' `assign` (tableVal arr (var "i") :: Exp) : (tableVal arr (var "i") :: Var) `assign` Nil :[]) es

pushValues :: String -> [Exp] -> [Stat]
pushValues arr exps = case exps of
    []  -> []
    [e] -> [(tableVal arr (Unop Len (var arr) + 1) :: Var) `assign` e]
    _   -> ("i" `lassign` Unop Len (var arr)) : zipWith (\n e -> (tableVal arr (var "i" + num n) :: Var) `assign` e) [1..length exps] (reverse exps)

generate :: [Binding] -> [Stat] -> [Stat] -> [TypeR] -> [Stat]
generate binds rts primops typs = runReader
    (evalStateT go (GeneratorState [] [] []))
        (GeneratorEnv (symbolExtract rts ++ idBlackList) [InlinePrimOps] (primOpsExtract primops) typs undefined luaNameObfuscator continuationWithSwitchCaseGen)
        -- (GeneratorEnv (symbolExtract rts ++ idBlackList) [] {- [InlinePrimOps] -} (primOpsExtract primops) typs undefined luaNameObfuscator continuationCaseGen)
        -- (GeneratorEnv (symbolExtract rts ++ idBlackList) [InlinePrimOps] (primOpsExtract primops) typs undefined luaNameObfuscator callCaseGen)
    where
        go :: Generate [Stat]
        go = do
            itbls  <- infoTablesGen $ appendUniq [] $ everywhere
                (mkT (const (Variable "")) `extT` (const (UnboxedVariable ""))) $ collectConstr binds
            binds' <- bindingsGen (binds)
            -- binds' <- bindingsGen (depSort binds)
            return (itbls ++ binds')

        primOpsExtract :: [Stat] -> [(String, FunBody)]
        primOpsExtract stats = concatMap go' stats
            where
                go' st = case st of
                    Assign [VarName nm] [EFunDef fb]     -> [(nm, fb)]
                    LocalAssign [nm] (Just [EFunDef fb]) -> [(nm, fb)]
                    _                                    -> []

        symbolExtract :: [Stat] -> [String]
        symbolExtract stats = concatMap go' stats
            where
                go' st = case st of
                    Assign vars _       -> concatMap varName vars
                    LocalAssign ns _    -> ns
                    LocalFunAssign nm _ -> [nm]
                    _                   -> []

                varName v = case v of
                    VarName n -> [n]
                    _         -> []

infoTablesGen :: [Constructor Label] -> Generate [Stat]
infoTablesGen conss = fmap concat $ forM conss $ \(Constructor name xs) -> do
    obf <- asks nameObfuscator
    top <- asks topLevelBindings
    loc <- gets (map snd . constrVariables)
    let obfName = obf (top ++ loc) name Nothing
    let stackVector
            | [] <- xs  = var obfName : var obfName : []
            | otherwise = var "self" : var obfName : map (tableVal "self" . num) [1..length xs]
    let cycleTable
            | [] <- xs  = [callFunByName "setmetatable" [var obfName, var obfName]]
            | otherwise = []
    modify $ \s -> s { constrVariables = ((name, obfName) : constrVariables s) }
    return $
        [ obfName `lassign` TableConst
            [ NamedField "name"   (String name)
            , NamedField "arity" (num (length xs)) ]
        , SelectName (var obfName) "enter" `assign` (EFunDef $ FunBody ["self"] False $
            map (\x -> callFunByName "PUSH" [x]) (reverse stackVector) `prependsBlock`
            exprBlock (callFunByName "JUMP" [callFunByName "POP_CONTROL" []]))
        ] ++ cycleTable

nameGen :: Label -> Maybe String -> Generate String
nameGen lbl mpref = do
    obf <- asks nameObfuscator
    top <- asks topLevelBindings
    loc <- gets (map snd . currentNameSpace)
    let vRef = (lbl, obf (top ++ loc) str mpref)
    modify $ \s -> s { currentNameSpace = (vRef : currentNameSpace s) }
    return (snd vRef)
    where
        str
            | Boxed (Variable s) <- lbl          = s
            | Unboxed (UnboxedVariable s) <- lbl = s
            | otherwise                          = undefined

labelGen :: Label -> Generate String
labelGen lbl = case lbl of
    Boxed _   -> nameGen lbl Nothing
    Unboxed _ -> nameGen lbl (Just "unboxed")

lookupVar :: Label -> Generate String
lookupVar lbl = do
    ns <- liftM2 (:) (gets currentNameSpace) (gets nameStack)
    case lookup lbl (concat ns) of
        Just v  -> return v
        Nothing -> do
            ns <- gets currentNameSpace
            error ("Var " ++ show lbl ++ " not found " ++ show ns)

lookupConstr :: String -> Generate String
lookupConstr name = do
    constrs <- gets constrVariables
    case lookup name constrs of
        Just v  -> return v
        Nothing -> error ("Constructor " ++ name ++ " not found")

atomGen :: Atom -> Generate Exp
atomGen at = case at of
    VarAtom lbl        -> liftM var $ lookupVar lbl
    LitAtom (IntLit i) -> return $ num i
    _                  -> undefined

createNamespace :: Generate a -> Generate a
createNamespace gen = do
    modify $ \st -> st
        { currentNameSpace = []
        , nameStack = (currentNameSpace st : nameStack st) }
    v           <- gen
    (ns:nstack) <- gets nameStack
    modify $ \st -> st { currentNameSpace = ns, nameStack = nstack }
    return v

bindingsGen :: [Binding] -> Generate [Stat]
bindingsGen binds = do
    names'  <- mapM (labelGen . Boxed . bindingName) binds
    exprs   <- forM binds $ \b -> case b of
        Binding name _fvars u _ vars expr
            | [] <- vars, not u, AtomExpr at <- expr -> atomGen at
            | [] <- vars, ApplyExpr f xs  <- expr    -> do
                name <- lookupVar (Boxed f)
                args <- mapM atomGen xs
                return (callFunByName "setmetatable" [TableConst (Field (var name) : map Field args), var (if u then "APPLY_THUNK_TABLE" else "APPLY_TABLE")])
            | [] <- vars, ConstrExpr (Constructor name []) <- expr -> do
                name' <- lookupConstr name
                return (var name')
            | [] <- vars, ConstrExpr (Constructor name atoms) <- expr -> do
                name' <- lookupConstr name
                atms' <- mapM atomGen atoms
                return (callFunByName "setmetatable" [TableConst (map Field atms'), var name'])
            | [] <- vars, u                          -> thunkGen (exprGen expr)
            | otherwise                              -> lambdaAbstractionGen vars (exprGen expr)
    return (zipWith assign names' exprs)

thunkGen :: Generate Block -> Generate Exp
thunkGen gen = createNamespace $ do
    blck <- gen
    return $ callFunByName "THUNK" [EFunDef (FunBody [] False blck)]

lambdaAbstractionGen :: [Label] -> Generate Block -> Generate Exp
lambdaAbstractionGen argv gen = createNamespace $ do
    argvRefs <- mapM labelGen argv
    blck     <- gen
    return $ case argv of
        []   -> EFunDef (FunBody argvRefs False blck)
        _ -> EFunDef (FunBody [] False (argvRefs `lassign` (replicate (length argvRefs) (callFunByName "POP" [])) `prependBlock` blck))
        -- []   -> callFunByName "FUNCTION0" [EFunDef (FunBody argvRefs False blck)]
        -- _ -> callFunByName "FUNCTION0" [EFunDef (FunBody [] False (argvRefs `lassign` (replicate (length argvRefs) (callFunByName "POP" [])) `prependBlock` blck))]
    -- return $ case argv of
    --     []   -> callFunByName "FUNCTION0" [EFunDef (FunBody argvRefs False blck)]
    --     _:[] -> callFunByName "FUNCTION1" [EFunDef (FunBody argvRefs False blck)]
    --     _    -> callFunByName "FUNCTION"  [num (length argv), EFunDef (FunBody argvRefs False blck)]

exprGen :: StgExpr -> Generate Block
exprGen expr = case expr of
    AtomExpr atom        -> atomGen atom >>= return . exprBlock
    ApplyExpr name argv  -> do
        f     <- lookupVar (Boxed name)
        argv' <- mapM atomGen argv
        return $ exprBlock $ callFunByName "APPLY" (var f : argv')
    ConstrExpr (Constructor name argv) -> do
        constr <- lookupConstr name
        argv'  <- mapM atomGen argv
        let stackVector
                | [] <- argv = var constr : var constr : []
                | otherwise  = var "NOT_ALLOC" : var constr : argv'
        return $
            map (\x -> callFunByName "PUSH" [x]) (reverse stackVector) `prependsBlock`
            exprBlock (callFunByName "JUMP" [callFunByName "POP_CONTROL" []])
    PrimOpExpr name argv -> do
        conf  <- asks generatorOpts
        pops  <- asks primOps
        argv' <- mapM atomGen argv
        return $ case conf of
            _
                | InlinePrimOps `elem` conf
                , Just (FunBody args _ blck) <- lookup name pops -> Block [Do (args `lassign` argv' `prependBlock` blck)] Nothing
                | otherwise                                      -> exprBlock $ callFunByName name argv'
    LetIn _ binds expr' -> liftM2 prependsBlock (bindingsGen binds) (exprGen expr')
    Case expr' alts     -> do
        gen <- asks caseExprGen
        gen expr' alts
    ThrowExpr str -> return $ Block [callFunByName "error" [String str]] Nothing

defaultVar :: Alts StgExpr -> Maybe Label
defaultVar alts = case alts of
    VarSingle v _                  -> Just v
    IntAlts xs
        | (VarPat v, _) <- last xs -> Just (Unboxed v)
    AlgAlts xs
        | (VarPat v, _) <- last xs -> Just (Boxed v)
    DoubleAlts xs
        | (VarPat v, _) <- last xs -> Just (Unboxed v)
    StringAlts xs
        | (VarPat v, _) <- last xs -> Just (Unboxed v)
    _                              -> Nothing

callCaseGen :: StgExpr -> Alts StgExpr -> Generate Block
callCaseGen expr alts = do
    ret   <- labelGen nodeLabel
    conf  <- asks generatorOpts
    sts   <- flip liftM (exprGen expr) $ case expr of
        PrimOpExpr _ _
            | InlinePrimOps `elem` conf -> \b -> (LocalAssign [ret] Nothing):statementify (ret `assign`) b
            | otherwise            -> statementify (ret `lassign`)
        AtomExpr (VarAtom (Unboxed _)) -> statementify (ret `lassign`)
        AtomExpr (LitAtom _)           -> statementify (ret `lassign`)
        _                              -> statementify $ \e -> ret `lassign` callFunByName "EVAL" [e]
    blck' <- case alts of
        VarSingle _ expr''  -> exprGen expr''
        Single expr''       -> exprGen expr''
        AlgAlts xs          -> ifThenElseAltsGen xs conditionAlgExprGen
        IntAlts xs          -> ifThenElseAltsGen xs conditionIntExprGen
    return (sts `prependsBlock` blck')
    where
        ifThenElseAltsGen ::
            [(Pattern a b, StgExpr)] ->
            (Pattern a b -> StgExpr -> Generate (Maybe Exp, Block)) -> Generate Block
        ifThenElseAltsGen xs gen = do
            v <- foldM go ([],Nothing) xs
            return $ case v of
                ([], Just end) -> end
                (ways, mend)   -> Block [If ways mend] Nothing
            where
                go v@(acc, mend) (pat, expr)
                    | Just _ <- mend = return v
                    | otherwise      = do
                        (mcond, blck) <- gen pat expr
                        return $ case mcond of
                            Nothing   -> (acc, Just blck)
                            Just cond -> (acc ++ [(cond, blck)], mend)

        conditionAlgExprGen :: Pattern Variable (Constructor Label) -> StgExpr -> Generate (Maybe Exp, Block)
        conditionAlgExprGen pat expr = case pat of
            Empty    -> liftM (\e -> (Nothing, e)) (exprGen expr)
            VarPat x -> liftM (\e -> (Nothing, e)) (exprGen expr)
            SPat (Constructor name vars) -> do
                con   <- lookupConstr name
                v     <- lookupVar nodeLabel
                vars' <- mapM labelGen vars
                expr' <- exprGen expr
                let resultExp i = PrefixExp (namedTableIndexedItem v (num i))
                return
                    ( Just (var con .== resultExp 1)
                    , vars' `lassign` map resultExp [2..1 + length vars'] `prependBlock` expr')

        conditionIntExprGen :: Pattern UnboxedVariable Integer -> StgExpr -> Generate (Maybe Exp, Block)
        conditionIntExprGen pat expr = case pat of
            Empty    -> liftM (\e -> (Nothing, e)) (exprGen expr)
            VarPat x -> liftM (\e -> (Nothing, e)) (exprGen expr)
            SPat i   -> do
                v     <- lookupVar nodeLabel
                expr' <- exprGen expr
                return (Just (num i .== var v), expr')

        nodeLabel :: Label
        nodeLabel = maybe (Boxed $ Variable "value") id (defaultVar alts)

continuationCaseGen :: StgExpr -> Alts StgExpr -> Generate Block
continuationCaseGen expr alts
    | PrimOpExpr _ _                 <- expr = callCaseGen expr alts
    | AtomExpr (LitAtom _)           <- expr = callCaseGen expr alts
    | AtomExpr (VarAtom (Unboxed _)) <- expr = callCaseGen expr alts
    | otherwise                              = do
        blck <- exprGen expr
        sts  <- hashTableAltsGen alts
        return (sts `prependsBlock` blck)
    where
        hashTableAltsGen :: Alts StgExpr -> Generate [Stat]
        hashTableAltsGen alts = case alts of
            Single expr -> do
                ex <- lambdaAbstractionGen [] (exprGen expr)
                return $ pushValues "RETURN_STACK" [var "DROP_STACK_ALL_TABLE", ex]
            VarSingle v expr -> do
                ex <- lambdaAbstractionGen [v] (exprGen expr)
                return $ pushValues "RETURN_STACK" [var "DROP_STACK_VAR_TABLE", ex]
            IntAlts xs  -> do
                xs' <- forM xs (uncurry hashTableIntAltsGen)
                return $ pushValues "RETURN_STACK" [var $ tableDispatch xs, TableConst xs']
            AlgAlts xs  -> do
                xs' <- forM xs (uncurry hashTableAlgAltsGen)
                return $ pushValues "RETURN_STACK" [var $ tableDispatch xs, TableConst xs']

        tableDispatch :: [(Pattern a b, c)] -> String
        tableDispatch xs = case last xs of
            (Empty, _)      -> "VECTOR_WITH_EMPTY_TABLE"
            (VarPat _, _)   -> "VECTOR_WITH_VAR_TABLE"
            _               -> "VECTOR_TABLE"

continuationWithSwitchCaseGen :: StgExpr -> Alts StgExpr -> Generate Block
continuationWithSwitchCaseGen expr alts
    | PrimOpExpr _ _                 <- expr = callCaseGen expr alts
    | AtomExpr (LitAtom _)           <- expr = callCaseGen expr alts
    | AtomExpr (VarAtom (Unboxed _)) <- expr = callCaseGen expr alts
    | otherwise                              = do
        blck <- exprGen expr
        sts  <- hashTableAltsGen alts
        return (sts `prependsBlock` blck)
    where
        hashTableAltsGen :: Alts StgExpr -> Generate [Stat]
        hashTableAltsGen alts = case alts of
            Single expr -> do
                e <- lambdaAbstractionGen [] (exprGen expr)
                return
                    [ callFunByName "PUSH_CONTROL" [e]
                    , callFunByName "PUSH_CONTROL" [var "DROP_STACK_ALL_TABLE"] ]
            VarSingle v expr -> do
                e <- lambdaAbstractionGen [v] (exprGen expr)
                return
                    [ callFunByName "PUSH_CONTROL" [e]
                    , callFunByName "PUSH_CONTROL" [var "DROP_STACK_VAR_TABLE"] ]
            IntAlts xs  -> do
                xs' <- forM xs (uncurry hashTableIntAltsGen)
                return $ reverse
                    [ callFunByName "PUSH_CONTROL" [var "VECTOR_TABLE"]
                    , callFunByName "PUSH_CONTROL" [TableConst xs'] ]
            AlgAlts xs  -> do
                e <- switchClosureGen xs constrPatternExprGen
                -- return $ map (\x -> callFunByName "PUSH_CONTROL" [x]) (reverse [var "SWITCH_TABLE", e])
                return [callFunByName "PUSH_CONTROL" [e]]

        switchClosureGen :: [(Pattern a b, StgExpr)] -> (b -> StgExpr -> Generate (Exp, Block)) -> Generate Exp
        switchClosureGen xs gen = createNamespace $ do
            node' <- labelGen $ maybe (Boxed $ Variable "heapRef") id (defaultVar alts)
            tag'  <- labelGen (Boxed $ Variable "tag")
            let isSpat x
                    |(SPat _, _) <- x = True
                    | otherwise       = False
            let (spats, dpats) = span isSpat xs
            let stackClean = ForRange "i"
                    (Unop Len (var "ARGUMENTS_STACK"))
                    (Unop Len (var "ARGUMENTS_STACK") - PrefixExp (PEVar (SelectName (var tag') "arity")) + 1)
                    (Just (-1))
                    (Block [(tableVal "ARGUMENTS_STACK" (var "i") :: Var) `assign` Nil] Nothing)
            let defWay
                    | (Empty, e):_ <- dpats     = liftM (Just . (stackClean `prependBlock`)) (exprGen e)
                    | (VarPat _, e):_ <- dpats  = do
                        e' <- exprGen e
                        return $ Just $ If [(var node' .== var "NOT_ALLOC", Block [node' `assign` (callFunByName "ALLOCATE" [var tag'])] Nothing)] (Just (Block [stackClean] Nothing)) `prependBlock`  e'
                    | otherwise                 = return Nothing
            ways  <- mapM (\(SPat c, e) -> gen c e) spats
            mblck <- defWay
            return $ EFunDef $ FunBody [] False (Block (map (\x -> x `lassign` callFunByName "POP" []) [node', tag'] ++ [If ways mblck]) Nothing)

        constrPatternExprGen :: Constructor Label -> StgExpr -> Generate (Exp, Block)
        constrPatternExprGen (Constructor name lbls) expr = createNamespace $ do
            tag'  <- lookupVar (Boxed (Variable "tag"))
            name' <- lookupConstr name
            lbls' <- mapM labelGen lbls
            expr' <- exprGen expr
            let varBinds
                    | [] <- lbls' = []
                    | otherwise   = map (\x -> x `lassign` callFunByName "POP" []) lbls'
            return (var name' .== var tag', varBinds `prependsBlock` expr')

hashTableAlgAltsGen :: Pattern Variable (Constructor Label) -> StgExpr -> Generate TableField
hashTableAlgAltsGen pat expr = case pat of
    Empty                        -> liftM (ExpField (var "DEFAULT")) (lambdaAbstractionGen [] $ exprGen expr)
    VarPat x                     -> liftM (ExpField (var "DEFAULT")) (lambdaAbstractionGen [Boxed x] $ exprGen expr)
    SPat (Constructor name vars) -> do
        name' <- lookupConstr name
        liftM (ExpField (var name')) (lambdaAbstractionGen vars $ exprGen expr)

hashTableIntAltsGen :: Pattern UnboxedVariable Integer -> StgExpr -> Generate TableField
hashTableIntAltsGen pat expr = case pat of
    Empty       -> liftM (ExpField (var "DEFAULT")) (lambdaAbstractionGen [] $ exprGen expr)
    VarPat x    -> liftM (ExpField (var "DEFAULT")) (lambdaAbstractionGen [Unboxed x] $ exprGen expr)
    SPat i      -> liftM (ExpField (num i)) (lambdaAbstractionGen [] $ exprGen expr)
