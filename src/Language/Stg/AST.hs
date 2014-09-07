module Language.Stg.AST where

-- base
import Data.Data
import Data.List
import Data.Maybe

-- syb
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Generics.Twins

newtype Variable = Variable String
    deriving (Show, Eq, Data, Typeable)

newtype UnboxedVariable = UnboxedVariable String
    deriving (Show, Eq, Data, Typeable)

data Label = Boxed Variable | Unboxed UnboxedVariable
    deriving (Show, Eq, Data, Typeable)

data Constructor a = Constructor String [a]
    deriving (Show, Eq, Data, Typeable)

data Atom = VarAtom Label | LitAtom Literal
    deriving (Show, Eq, Data, Typeable)

data Literal = IntLit Integer | DoubleLit Double | StringLit String
    deriving (Show, Eq, Data, Typeable)

data TypeVal = UnbT UnboxedT | AlgT (Constructor Label)
    deriving (Show, Eq, Data, Typeable)

type TypeR = (Constructor Label, [Constructor TypeVal])

data UnboxedT = IntT | DoubleT | StringT
    deriving (Show, Eq, Data, Typeable)

data Binding = Binding
    { bindingName :: Variable
    , bindingFreeVars :: [Label]
    , updateable :: Bool
    , bindingProperty :: [BindingProperty]
    , bindingVars :: [Label]
    , bindingExpr :: StgExpr }
    deriving (Show, Eq, Data, Typeable)

data BindingProperty = Recursive | Constant
    deriving (Show, Eq, Data, Typeable)

data Alts a
    = Single a
    | VarSingle Label a
    | IntAlts    [(Pattern UnboxedVariable Integer, a)]
    | DoubleAlts [(Pattern UnboxedVariable Double, a)]
    | StringAlts [(Pattern UnboxedVariable String, a)]
    | AlgAlts    [(Pattern Variable (Constructor Label), a)]
    deriving (Show, Eq, Data, Typeable)

data Pattern v s = Empty | VarPat v | SPat s
    deriving (Show, Eq, Data, Typeable)

data Alts' v s = Alts' [(s, StgExpr)] (Maybe (Def v))
    deriving (Show, Eq, Data, Typeable)

data Def v = Empty' StgExpr | VarPat' v StgExpr
    deriving (Show, Eq, Data, Typeable)

type ConstrPat = (String, [Label])

data StgExpr
    = AtomExpr Atom
    | ApplyExpr Variable [Atom]
    | ConstrExpr (Constructor Atom)
    | PrimOpExpr String [Atom]
    | LetIn Bool [Binding] StgExpr
    | Case StgExpr (Alts StgExpr)
    | ThrowExpr String
    deriving (Show, Eq, Data, Typeable)

appendUniq :: Eq a => [a] -> [a] -> [a]
appendUniq = foldl (\acc v -> if v `notElem` acc then (v:acc) else acc)

deriveFreeVars :: Data b => [Label] -> b -> b
deriveFreeVars unfree = everywhere $ mkT $
    \bind -> bind { bindingFreeVars = collectFreeVars (Boxed (bindingName bind) : unfree) (bindingExpr bind) }

collectFreeVars :: Data a => [Label] -> a -> [Label]
collectFreeVars unfree = snd . everythingWithContextBut unfree appendUniq
    (mkQ (\s -> ([], False, s))
        fromBindings `extQ`
        fromPatternCon `extQ`
        fromPatternInt `extQ`
        fromPatternDouble `extQ`
        fromPatternString `extQ`
        (\lb s -> ([lb] \\ s, True, s)) `extQ`
        (\v s -> ([Boxed v] \\ s, True, s)) `extQ`
        (\uv s -> ([Unboxed uv] \\ s, True, s)))
    where
        everythingWithContextBut :: s -> (r -> r -> r) -> GenericQ (s -> (r, Bool, s)) -> GenericQ (s, r)
        everythingWithContextBut st k q x
            | stop      = (st', v)
            | otherwise = gmapAccumQl k v (\acc e -> everythingWithContextBut acc k q e) st' x
            where
                (v, stop, st') = q x st

        fromBindings :: [Binding] -> [Label] -> ([Label], Bool, [Label])
        fromBindings binds unfree' =
            (concatMap bindingFreeVars binds \\ unfree'', True, unfree'')
            where
                unfree'' = unfree' `appendUniq` map (Boxed . bindingName) binds

        fromPatternCon :: Pattern Variable (Constructor Label) -> [Label] -> ([Label], Bool, [Label])
        fromPatternCon pat unfree' = ([], True, unfree' `appendUniq` collectVariable pat)

        fromPatternInt :: Pattern UnboxedVariable Integer -> [Label] -> ([Label], Bool, [Label])
        fromPatternInt pat unfree' = ([], True, unfree' `appendUniq` collectVariable pat)

        fromPatternDouble :: Pattern UnboxedVariable Double -> [Label] -> ([Label], Bool, [Label])
        fromPatternDouble pat unfree' = ([], True, unfree' `appendUniq` collectVariable pat)

        fromPatternString :: Pattern UnboxedVariable String -> [Label] -> ([Label], Bool, [Label])
        fromPatternString pat unfree' = ([], True, unfree' `appendUniq` collectVariable pat)

collectConstr :: Data a => a -> [Constructor Label]
collectConstr = everything appendUniq (mkQ [] return `extQ` fromExpr)
    where
        fromExpr :: Constructor Atom -> [Constructor Label]
        fromExpr (Constructor name atoms) = [Constructor name (map atomTypeVar atoms)]

        atomTypeVar :: Atom -> Label
        atomTypeVar atom = case atom of
            LitAtom _ -> Unboxed (UnboxedVariable "x")
            VarAtom v -> v

collectVariable :: Data a => a -> [Label]
collectVariable = everything appendUniq
    (mkQ [] return `extQ` (return . Boxed) `extQ` (return . Unboxed))

cutAltsTransform :: Data a => a -> a
cutAltsTransform = everywhere $ mkT
    (\a -> cutAltsTransform' a :: [(Pattern Variable (Constructor Label), StgExpr)]) `extT`
    (\a -> cutAltsTransform' a :: [(Pattern UnboxedVariable Integer, StgExpr)]) `extT`
    (\a -> cutAltsTransform' a :: [(Pattern UnboxedVariable Double, StgExpr)]) `extT`
    (\a -> cutAltsTransform' a :: [(Pattern UnboxedVariable String, StgExpr)])
    where
        cutAltsTransform' :: [(Pattern a b, StgExpr)] -> [(Pattern a b, StgExpr)]
        cutAltsTransform' alts = case alts of
            []                     -> [] -- [(Empty, ThrowExpr "pattern match fail")]
            (x:xs)
                | (SPat _, _) <- x -> (x:cutAltsTransform' xs)
                | otherwise        -> [x]

singleCaseTransform :: Data a => a -> a
singleCaseTransform = everywhere $ mkT singleCaseTransform'
    where
        singleCaseTransform' :: Alts StgExpr -> Alts StgExpr
        singleCaseTransform' alts = case alts of
            IntAlts xs
                | [(Empty, v)] <- xs    -> Single v
                | [(VarPat p, v)] <- xs -> VarSingle (Unboxed p) v
            DoubleAlts xs
                | [(Empty, v)] <- xs    -> Single v
                | [(VarPat p, v)] <- xs -> VarSingle (Unboxed p) v
            StringAlts xs
                | [(Empty, v)] <- xs    -> Single v
                | [(VarPat p, v)] <- xs -> VarSingle (Unboxed p) v
            AlgAlts xs
                | [(Empty, v)] <- xs    -> Single v
                | [(VarPat p, v)] <- xs -> VarSingle (Boxed p) v
            _                           -> alts

isConstant :: [Binding] -> Binding -> Bool
isConstant context bind
    | _:_ <- bindingVars bind                   = True
    | []  <- collectVariable (bindingExpr bind) = True
    | isRecursive context bind                  = False
    | otherwise                                 = False

isRecursive :: [Binding] -> Binding -> Bool
isRecursive context bind
    | Boxed (bindingName bind) `elem` allVars [] bind = True
    | otherwise                                  = False
    where
        allVars :: [Label] -> Binding -> [Label]
        allVars unfree b = let xs = collectFreeVars (unfree `appendUniq` bindingVars b) (bindingExpr b) in
            xs ++ concatMap (allVars (unfree `appendUniq` xs)) (concatMap selectBind xs)

        selectBind :: Label -> [Binding]
        selectBind lbl
            | Boxed var <- lbl
            , Just v <- lookup var (map (\b -> (bindingName b, b)) context) = [v]
            | otherwise                                                     = []

betaReductionTransform :: Binding -> [Label] -> StgExpr
betaReductionTransform bind vars
    | [] <- bindingVars bind                  = bindingExpr bind
    | length (bindingVars bind) > length vars = undefined
    | otherwise                               =
        foldl (\cexpr (old, new) -> renameLabelTransform cexpr old new)
            (bindingExpr sanitizedBinding) (zip (bindingVars sanitizedBinding) vars)
    where
        mkPlaceHolder :: Label -> Label
        mkPlaceHolder lbl
            | lbl `notElem` vars = lbl
            | otherwise  = mkPlaceHolder (everywhere (mkT (\str -> '0':str)) lbl)

        sanitizedBinding :: Binding
        sanitizedBinding = foldl sanitizeBinding bind (bindingVars bind)

        sanitizeBinding :: Binding -> Label -> Binding
        sanitizeBinding cbind lbl
            | lbl `notElem` vars = cbind
            | otherwise  = cbind
                { bindingExpr = renameLabelTransform (bindingExpr cbind) lbl (mkPlaceHolder lbl)
                , bindingVars = map (\v -> if v == lbl then (mkPlaceHolder lbl) else v) (bindingVars cbind)
                }

renameLabelTransform :: StgExpr -> Label -> Label -> StgExpr
renameLabelTransform expr oldname newname
    | oldname == newname = expr
    | otherwise         =
        everywhereBut
            (mkQ False
                bindingOverlapped `extQ`
                notCaptured `extQ`
                algPatternOverlapped `extQ`
                integerPatternOverlapped `extQ`
                stringPatternOverlapped `extQ`
                doublePatternOverlapped)
            (mkT work)
            expr
    where
        notCaptured :: Binding -> Bool
        notCaptured bind = oldname `notElem` bindingFreeVars bind

        bindingOverlapped :: StgExpr -> Bool
        bindingOverlapped e
            | LetIn _ bs _ <- e = oldname `elem` map (Boxed . bindingName) bs
            | otherwise         = False

        algPatternOverlapped :: (Pattern Variable (Constructor Label), StgExpr) -> Bool
        algPatternOverlapped (pat, _) = oldname `elem` collectVariable pat

        integerPatternOverlapped :: (Pattern UnboxedVariable Integer, StgExpr) -> Bool
        integerPatternOverlapped (pat, _) = oldname `elem` collectVariable pat

        doublePatternOverlapped :: (Pattern UnboxedVariable Double, StgExpr) -> Bool
        doublePatternOverlapped (pat, _) = oldname `elem` collectVariable pat

        stringPatternOverlapped :: (Pattern UnboxedVariable String, StgExpr) -> Bool
        stringPatternOverlapped (pat, _) = oldname `elem` collectVariable pat

        work :: Label -> Label
        work cname
            | cname == oldname = newname
            | otherwise       = cname

deadTopBindingElimination :: [Binding] -> [Binding]
deadTopBindingElimination binds
    | Just v <- find (\v -> bindingName v == Variable "main") binds = v : deadTopBindingEliminationTransformation v (binds \\ [v])
    | otherwise                                                    = undefined

deadTopBindingEliminationTransformation :: Binding -> [Binding] -> [Binding]
deadTopBindingEliminationTransformation entry binds = query ++ concatMap (\n -> deadTopBindingEliminationTransformation n (binds \\ query)) query
    where
        fVars  = collectFreeVars [] entry
        query  = catMaybes $ map (\lbl -> find (\v -> Boxed (bindingName v) == lbl) binds) fVars

depSort :: [Binding] -> [Binding]
depSort bs = case bs of
    (x:xs)
        | [] <- collectFreeVars [] x `intersect` map (Boxed . bindingName) xs -> x : depSort xs
        | otherwise                                                           -> depSort (xs ++ [x])
    _                                                                         -> []
