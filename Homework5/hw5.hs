{-# LANGUAGE TypeSynonymInstances #-}
{-# Language FlexibleInstances    #-}
import ExprT
import Parser
import qualified StackVM as S
import qualified Data.Map as M


-- data ExprT = Lit Integer
--            | Add ExprT ExprT
--            | Mul ExprT ExprT
--   deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add lhs rhs) = eval lhs + eval rhs
eval (Mul lhs rhs) = eval lhs * eval rhs

evalStr :: String -> Maybe Integer
evalStr ss = case ast of
            (Just ast') -> Just (eval ast')
            (Nothing) -> Nothing

    where 
        ast = parseExp Lit Add Mul ss

class Expr a where
    mul :: a -> a -> a
    add :: a -> a -> a
    lit :: Integer -> a

instance Expr ExprT where
    mul lhs rhs = Mul lhs rhs
    add lhs rhs = Add lhs rhs
    lit v = Lit v

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    mul lhs rhs = (*) lhs rhs
    add lhs rhs = (+) lhs rhs
    lit v =  v

instance Expr Bool where
    lit v = if v <= 0 then False else True
    mul lhs rhs = lhs && rhs
    add lhs rhs = lhs || rhs

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit v = MinMax v
    mul (MinMax lhs) (MinMax rhs) = (MinMax $ max lhs rhs)
    add (MinMax lhs) (MinMax rhs) = (MinMax $ min lhs rhs)

instance Expr Mod7 where
    lit v = Mod7 (v `mod` 7)
    mul (Mod7 lhs) (Mod7 rhs) = Mod7 $ mod (lhs * rhs) 7
    add (Mod7 lhs) (Mod7 rhs) = Mod7 $ mod (lhs + rhs) 7


instance Expr S.Program where
    lit v = [S.PushI v]
    mul lhs rhs = lhs ++ rhs ++ [S.Mul]
    add lhs rhs = lhs ++ rhs ++ [S.Add]

compile :: String -> Maybe S.Program   
compile = parseExp lit add mul

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 6"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7



--ex6
data VarExprT =  VLit Integer
                | VAdd VarExprT VarExprT
                | VMul VarExprT VarExprT
                | VVar String
        deriving (Show, Eq)

-- instance Expr VarExprT where
--     lit = VLit
--     add = VAdd
--     mul = VMul

-- instance HasVars VarExprT where
--     var = VVar

class HasVars a where
    var :: String -> a

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var m = M.lookup m

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit a = \_ -> Just a
    add a b = (\vs -> (+) <$> (a vs) <*> (b vs))
    mul a b = (\vs -> (*) <$> (a vs) <*> (b vs))


withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs