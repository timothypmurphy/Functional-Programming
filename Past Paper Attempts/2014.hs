

-- Informatics 1 Functional Programming
-- December 2014
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck,
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM2, liftM3, used below
import Data.Char

main :: IO ()
main = return ()

-- Question 1

-- 1a

f :: [Int] -> Bool
f (x:xs) | null (x:xs) = error "Error"
         | otherwise = and[(mod a b) == 0 |(a,b) <- zip xs (x:xs)]

-- 1b

g :: [Int] -> Bool
g [] = True
g (x:y:xs) | x `mod` y == 0 = g (y:xs)
           | otherwise = True
g (x:[]) = True

propfg :: [Int] -> Bool
propfg xs = f xs == g xs

-- Question 2

-- 2a

p :: [Int] -> Int
p xs = product[x^2 | x <- xs, x<0]

-- 2b

q :: [Int] -> Int
q [] = 1
q (x:xs) | x < 0 = x^2 * q xs
         | otherwise = q xs

-- 2c

r :: [Int] -> Int
r xs = foldr (*) 1 (map (^2) (filter (<0) xs))

-- Question 3

data Expr = X
          | Const Int
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | IfZero Expr Expr Expr
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :-: q)  =  "(" ++ showExpr p ++ "-" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"
showExpr (p :/: q)  =  "(" ++ showExpr p ++ "/" ++ showExpr q ++ ")"
showExpr (IfZero p q r)  = "(if " ++ showExpr p ++ "=0 then "
                                  ++ showExpr q ++ " else "
                                  ++ showExpr r ++ ")"

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [X]]
                 | otherwise  =  oneof [ liftM Const arbitrary
                                       , liftM2 (:+:) subform2 subform2
                                       , liftM2 (:-:) subform2 subform2
                                       , liftM2 (:*:) subform2 subform2
                                       , liftM2 (:/:) subform2 subform2
                                       , liftM3 (IfZero) subform3 subform3 subform3
                                       ]
                 where
                   subform2  =  expr (n `div` 2)
                   subform3  =  expr (n `div` 3)

-- 3a

eval :: Expr -> Int -> Int
eval (IfZero p q r) x = if (eval p x) == 0 then (eval q x) else (eval r x)
eval (p :+: q) x = (eval p x) + (eval q x)
eval (p :-: q) x = (eval p x) - (eval q x)
eval (p :*: q) x = (eval p x) * (eval q x)
eval (p :/: q) x = (eval p x) `div` (eval q x)
eval (Const p) x = p
eval (p) x = x
-- 3 b

protect :: Expr -> Expr
protect = undefined
