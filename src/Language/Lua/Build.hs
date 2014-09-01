module Language.Lua.Build where

-- base
import Data.Data

-- language-lua
import Language.Lua
import Language.Lua.Syntax as Lua

-- syb
import Data.Generics.Aliases
import Data.Generics.Schemes

instance Num Exp where
    l + r         = Binop Add l r
    l - r         = Binop Sub l r
    l * r         = Binop Mul l r
    negate e      = Unop Neg e
    abs e         = PrefixExp $ PEFunCall $ NormalFunCall (PEVar (SelectName (var "math") "abs")) (Args [e])
    signum e      = undefined
    fromInteger i = Number (show i)

class VarRet a where
    var :: String -> a

instance VarRet Exp where
    var = PrefixExp . PEVar . VarName

instance VarRet Var where
    var = VarName

instance VarRet PrefixExp where
    var = PEVar . VarName

num :: (Show a, Num a) => a -> Exp
num = Number . show

class FunCallRet a where
    funCallToRet :: FunCall -> a

instance FunCallRet Exp where
    funCallToRet = PrefixExp . PEFunCall

instance FunCallRet Stat where
    funCallToRet = FunCall

class AssignArgs a b | a -> b where
    assignArgs :: a -> b -> ([Var], [Exp])

instance AssignArgs String Exp where
    assignArgs str expr = ([VarName str], [expr])

instance AssignArgs Var Exp where
    assignArgs v expr = ([v], [expr])

class LocalAssignArgs a b | a -> b where
    localAssignArgs :: a -> b -> ([Name], [Exp])

instance LocalAssignArgs String Exp where
    localAssignArgs str expr = ([str], [expr])

instance LocalAssignArgs [String] [Exp] where
    localAssignArgs strs exprs = (strs, exprs)

callFunByName :: FunCallRet r => String -> [Exp] -> r
callFunByName name vars = funCallToRet $ NormalFunCall (PEVar (VarName name)) (Args vars)

assign :: AssignArgs a b => a -> b -> Stat
assign larg rarg
    | [] <- vars, [] <- exprs = EmptyStat
    | otherwise               = Assign vars exprs
    where
        (vars, exprs) = assignArgs larg rarg

lassign :: LocalAssignArgs a b => a -> b -> Stat
lassign larg rarg
    | [] <- names, [] <- exprs = EmptyStat
    | otherwise                = LocalAssign names (Just exprs)
    where
        (names, exprs) = localAssignArgs larg rarg

namedTableIndexedItem :: String -> Exp -> PrefixExp
namedTableIndexedItem name ix = PEVar (Select (PEVar (VarName name)) ix)

class TableRet a where
    tableRet :: Var -> a

instance TableRet Var where
    tableRet = id

instance TableRet PrefixExp where
    tableRet = PEVar

instance TableRet Exp where
    tableRet = PrefixExp . PEVar

tableVal :: TableRet a => String -> Exp -> a
tableVal name e = tableRet (Select (PEVar (VarName name)) e)

prependBlock :: Stat -> Block -> Block
prependBlock st (Block sts mret) = Block (st : sts) mret

prependsBlock :: [Stat] -> Block -> Block
prependsBlock sts (Block sts' mret) = Block (sts ++ sts') mret

exprBlock :: Exp -> Block
exprBlock expr = Block [] (Just [expr])

exprsBlock :: [Exp] -> Block
exprsBlock expr = Block [] (Just expr)

(.==) :: Exp -> Exp -> Exp
(.==) lexp rexp = Binop Lua.EQ lexp rexp

statementify :: (Exp -> Stat) -> Block -> [Stat]
statementify f blck
    | Block stats _ <- everywhereBut (mkQ False cond) (mkT trans) blck = stats
    where
        cond :: FunBody -> Bool
        cond = const True

        trans :: Block -> Block
        trans blck = case blck of
            Block sts (Just exprs) -> Block (sts ++ map f exprs) Nothing
            _                      -> blck

mapExit :: (Exp -> Exp) -> Block -> Block
mapExit f blck = everywhereBut (mkQ False condition) (mkT transform) blck
    where
        condition :: FunBody -> Bool
        condition = const True

        transform :: Block -> Block
        transform blck
            | Block sts (Just exprs) <- blck = Block sts (Just (map f exprs))
            | otherwise                      = blck
