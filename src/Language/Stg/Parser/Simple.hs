module Language.Stg.Parser.Simple where

-- base
import Control.Monad

import Data.Functor

-- parsec
import Text.Parsec hiding ( Empty )
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

-- internal
import Language.Stg.AST

stgDef :: LanguageDef st
stgDef = haskellDef
    { identStart  = lower
    , identLetter = identLetter haskellStyle }

stg :: TokenParser st
stg = (makeTokenParser stgDef)
    { integer = lexeme stg $ do
        v <- negative <|> zero <|> positive
        _ <- char '#'
        return v
    }
    where
        negative = char '-' >> positive >>= \v -> return ((-1) * v)

        zero = char '0' >> return 0

        positive = liftM2 (:) (oneOf ['1'..'9']) (many digit) >>= return . read

commaSepList :: Parser a -> Parser [a]
commaSepList p = braces stg (commaSep stg p)

semiSepList1 :: Parser a -> Parser [a]
semiSepList1 p = braces stg $ many1 $ do
    v <- lexeme stg p
    _ <- semi stg
    return v

bracedStg :: TokenParser st
bracedStg = makeTokenParser stgDef

programStg :: Parser [Binding]
programStg = endBy1
    (whiteSpace stg >> bindClosureStg)
        (oneOfP [eof, void newline, void (symbol stg ";")])

exprStg :: Parser StgExpr
exprStg = oneOfP
    [throwExprParser, caseExprStg, applyExprStg, letrecInExprStg, letInExprStg, constrExprStg, AtomExpr <$> atom]
    where
        letInExprStg :: Parser StgExpr
        letInExprStg = do
            reserved stg "let"
            cls <- semiSepList1 bindClosureStg
            reserved stg "in"
            e   <- exprStg
            return (LetIn False cls e)

        letrecInExprStg :: Parser StgExpr
        letrecInExprStg = do
            reserved stg "letrec"
            cls <- semiSepList1 bindClosureStg
            reserved stg "in"
            e   <- exprStg
            return (LetIn True cls e)

        constrExprStg :: Parser StgExpr
        constrExprStg = liftM2
            (\ident ats -> ConstrExpr (Constructor ident ats))
                (lexeme stg constrIdentifier) (commaSepList atom)

        applyExprStg :: Parser StgExpr
        applyExprStg = liftM2 ApplyExpr (Variable <$> identifier stg) (commaSepList atom)

        caseExprStg :: Parser StgExpr
        caseExprStg = do
            reserved stg "case"
            at <- (AtomExpr . VarAtom) <$> variable <|> try (parens stg primOp) <|> (parens stg exprStg)
            reserved stg "of"
            alts <- oneOfP
                [ Single <$> singleClause
                , IntAlts <$> clauseParser integerPatternClause unboxVariableCaseClause
                , AlgAlts <$> clauseParser algPatternClause variableCaseClause ]
            return (Case at alts)

        primOp :: Parser StgExpr
        primOp = lexeme stg $ do
            name <- lexeme stg $ do
                s <- liftM2 (:) lower (many (alphaNum <|> char '_'))
                _ <- string "##"
                return s
            lst  <- commaSepList atom
            return (PrimOpExpr name lst)

        singleClause :: Parser StgExpr
        singleClause = braces stg $ do
            _ <- lexeme stg (string "_")
            reservedOp stg "->"
            e <- lexeme stg exprStg
            _ <- semi stg
            return e

        clauseParser :: Parser (Pattern a b) -> Parser (Pattern a b) -> Parser [(Pattern a b, StgExpr)]
        clauseParser p var = semiSepList1 $ do
            clause <- oneOfP [defaultCaseClause, var, p]
            reservedOp stg "->"
            e      <- exprStg
            return (clause, e)

        defaultCaseClause :: Parser (Pattern a b)
        defaultCaseClause = lexeme stg (string "_") >> return Empty

        variableCaseClause :: Parser (Pattern Variable b)
        variableCaseClause = liftM (VarPat . Variable) (identifier stg)

        unboxVariableCaseClause :: Parser (Pattern UnboxedVariable b)
        unboxVariableCaseClause = lexeme stg $ do
            var <- liftM2 (:) (identStart stgDef) (many (identLetter stgDef))
            _   <- char '#'
            return (VarPat (UnboxedVariable var))

        integerPatternClause :: Parser (Pattern v Integer)
        integerPatternClause = liftM SPat (integer stg)

        algPatternClause :: Parser (Pattern v (Constructor Label))
        algPatternClause = liftM2 (\n ls -> SPat (Constructor n ls)) (lexeme stg constrIdentifier) (commaSepList variable)

        throwExprParser :: Parser StgExpr
        throwExprParser = reserved stg "error" >> stringLiteral stg >>= return . ThrowExpr

oneOfP :: [Parser a] -> Parser a
oneOfP ps = case ps of
    []      -> error "too few parsers"
    [p]     -> try p
    (p:ps') -> try p <|> oneOfP ps'

bindClosureStg :: Parser Binding
bindClosureStg = do
    var      <- Variable <$> identifier stg
    reservedOp stg "="
    reservedOp stg "\\"
    upd      <- lexeme stg (liftM (=='u') (oneOf "un"))
    vars     <- commaSepList variable
    reservedOp stg "->"
    e        <- exprStg
    return (Binding var [] upd [] vars e)

variable :: Parser Label
variable = lexeme stg $ do
    var <- liftM2 (:) (identStart stgDef) (many (identLetter stgDef))
    option (Boxed (Variable var)) (char '#' >> return (Unboxed (UnboxedVariable var)))

atom :: Parser Atom
atom = oneOfP
    [ VarAtom <$> variable
    , LitAtom . IntLit  <$> integer stg ]

constrIdentifier :: Parser String
constrIdentifier = liftM2 (:) upper (many (letter <|> char '_'))
