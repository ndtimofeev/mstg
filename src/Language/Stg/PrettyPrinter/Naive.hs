module Language.Stg.PrettyPrinter.Naive where

-- base
import Data.List

-- internal
import Language.Stg.AST

shift :: [String] -> [String]
shift = map ("    " ++)

semicolon :: [String] -> [String]
semicolon xs = init xs ++ [last xs ++ ";"]

semilist :: (a -> String) -> [a] -> String
semilist f xs = "{" ++ intercalate "," (map f xs) ++ "}"

prepend :: String -> [String] -> [String]
prepend str (x:xs) = (str ++ " " ++ x):shift xs

data StgPrinterConf = ShowFreeVars
    deriving (Eq, Show)

pprintStg :: [StgPrinterConf] -> [Binding] -> String
pprintStg cfgs = intercalate "\n\n" . map (intercalate "\n" . pprintBinding cfgs)

pprintBinding :: [StgPrinterConf] -> Binding -> [String]
pprintBinding cfgs (Binding name fvars u _ vars expr) = semicolon $ (concat $
    [ pprintVariable name
    , " = "
    , if ShowFreeVars `elem` cfgs then semilist pprintLabel fvars ++ " " else ""
    , "\\"
    , if u then "u" else "n"
    , " "
    , semilist pprintLabel vars
    , " ->" ] ++ end1) : shift end2
    where
        pexpr = pprintExpr cfgs expr
        (end1, end2)
            | length pexpr > 1 = ([], pexpr)
            | otherwise        = (" ":pexpr, [])

pprintExpr :: [StgPrinterConf] -> StgExpr -> [String]
pprintExpr cfgs expr = case expr of
    AtomExpr at                     -> [pprintAtom at]
    ApplyExpr v as                  -> [pprintVariable v ++ " " ++ semilist pprintAtom as]
    ConstrExpr (Constructor nm as)  -> [nm ++ " " ++ semilist pprintAtom as]
    PrimOpExpr str as               -> [str ++ "## "  ++ semilist pprintAtom as]
    LetIn _ bs expr'                ->
        let
            pbs    = concatMap (pprintBinding cfgs) bs
            pexpr' = pprintExpr cfgs expr'
            (end1, end2)
                | length pbs < 2, length pexpr' < 2 = (" " ++ concat pbs ++ " } in " ++ concat pexpr', [])
                | length pbs < 2                    = (" " ++ concat pbs ++ " } in",shift pexpr')
                | otherwise                         = ("", shift pbs ++ ["} in " ++ head pexpr'] ++ shift (tail pexpr'))
        in ("let {" ++ end1) : end2
    Case (AtomExpr at) alts         -> ("case " ++ pprintAtom at ++ " of {"):shift (pprintAlts cfgs alts) ++ ["}"]
    Case e alts                     ->
        let
            pe = pprintExpr cfgs e
            cas'
                | length pe < 2 = ["case ( " ++ concat pe ++ " ) of {"]
                | otherwise     = ["case ("] ++ shift pe ++ [") of {"]
        in cas' ++ shift (pprintAlts cfgs alts) ++  ["}"]
    ThrowExpr st                    -> ["error \"" ++ st ++ "\""]
    _                               -> ["<m0>"]

pprintAlts :: [StgPrinterConf] -> Alts StgExpr -> [String]
pprintAlts cfgs alts = case alts of
    Single e      -> semicolon (prepend "_ ->" (pprintExpr cfgs e))
    VarSingle v e -> semicolon (prepend (pprintLabel v ++ " -> ") (pprintExpr cfgs e))
    IntAlts xs    -> concatMap (pprintCaseWay cfgs pprintUnboxedVariable (\v -> show v ++ "#")) xs
    DoubleAlts xs -> concatMap (pprintCaseWay cfgs pprintUnboxedVariable (\v -> show v ++ "#")) xs
    StringAlts xs -> concatMap (pprintCaseWay cfgs pprintUnboxedVariable (\v -> show v ++ "#")) xs
    AlgAlts xs    -> concatMap (pprintCaseWay cfgs pprintVariable (\(Constructor n lbls) -> n ++ " {" ++ intercalate "," (map pprintLabel lbls) ++ "}")) xs

pprintCaseWay :: [StgPrinterConf] -> (a -> String) -> (b -> String) -> (Pattern a b, StgExpr) -> [[Char]]
pprintCaseWay cfgs pprintVar pprintSPat (pat, expr) = semicolon $ (pprintPattern ++ " -> " ++ end1) : shift end2
    where
        pexpr = pprintExpr cfgs expr
        (end1, end2)
            | e1:es <- pexpr = (e1, es)
            | otherwise      = (concat pexpr, [])

        pprintPattern = case pat of
            Empty    -> "_"
            VarPat v -> pprintVar v
            SPat s   -> pprintSPat s

pprintVariable :: Variable -> String
pprintVariable (Variable str) = str

pprintUnboxedVariable :: UnboxedVariable -> String
pprintUnboxedVariable (UnboxedVariable str) = str ++ "#"

pprintLabel :: Label -> String
pprintLabel lbl = case lbl of
    Boxed v     -> pprintVariable v
    Unboxed uv  -> pprintUnboxedVariable uv

pprintLiteral :: Literal -> String
pprintLiteral lit = case lit of
    IntLit i -> show i ++ "#"
    DoubleLit d -> show d ++ "#"
    StringLit s -> "\"" ++ s ++ "\"#"

pprintAtom :: Atom -> String
pprintAtom at = case at of
    VarAtom lbl -> pprintLabel lbl
    LitAtom lit -> pprintLiteral lit
