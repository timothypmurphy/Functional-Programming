-- Informatics 1 Functional Programming
-- December 2013
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck,
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char

main :: IO ()
main = return ()

-- Question 1

-- 1a

f :: String -> Int
f xs = sum[digitToInt x * (3 ^ i) | (x,i) <- zip (reverse xs) [0..]]

testf =
  f "201" == 19 &&
  f "12" == 5 &&
  f "1202" == 47 &&
  f "120221" == 430



-- 1b

g :: String -> Int
g xs = g' (reverse xs) 0
  where
    g' [] i = 0
    g' (y:ys) i = digitToInt y * (3 ^ i) + (g' ys (i+1))

testg =
  g "201" == 19 &&
  g "12" == 5 &&
  g "1202" == 47 &&
  g "120221" == 430


-- Question 2

-- 2a

p :: [Int] -> Bool
p xs | not(null xs) && xs !! 0 /= 0 = and[a `mod` x == 0 | (a,x) <- zip (xs) (replicate(length xs) (head xs)), a >0]
     | otherwise = error "error"

testp =
  p [2,6,-3,0,18,-17,10] == True &&
  p [-13] == True &&
  p [-3,6,1,-3,9,18] == False &&
  p [5,-2,-6,3] == False


-- 2b
divby :: Int -> Int -> Bool
x `divby` a = (x `mod` a == 0)

q :: [Int] -> Bool
q [] = error "error"
q (x:xs) | x == 0 = error "error"
         | otherwise = q' x xs
  where
    q' z [] = True
    q' z (y:ys) | y > 0 = y `divby` z && q' z ys
                | otherwise = q' z ys


testq =
  q [2,6,-3,0,18,-17,10] == True &&
  q [-13] == True &&
  q [-3,6,1,-3,9,18] == False &&
  q [5,-2,-6,3] == False

-- 2c

r :: [Int] -> Bool
r [] = error "error"
r (x:xs) | x /= 0 = foldr (&&) True (map (\y -> y `mod` x == 0) (filter (\y -> y > 0) xs))
         | otherwise = error "error"

testr =
  r [2,6,-3,0,18,-17,10] == True &&
  r [-13] == True &&
  r [-3,6,1,-3,9,18] == False &&
  r [5,-2,-6,3] == False

-- Question 3

data Expr = X
          | Const Int
          | Neg Expr
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (Neg p)    =  "(-" ++ showExpr p ++ ")"
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- evaluate an Expr, given a value of X

evalExpr :: Expr -> Int -> Int
evalExpr X v          =  v
evalExpr (Const n) _  =  n
evalExpr (Neg p) v    =  - (evalExpr p v)
evalExpr (p :+: q) v  =  (evalExpr p v) + (evalExpr q v)
evalExpr (p :*: q) v  =  (evalExpr p v) * (evalExpr q v)

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [X]]
                 | otherwise  =  oneof [ liftM Const arbitrary
                                       , liftM Neg subform
                                       , liftM2 (:+:) subform subform
                                       , liftM2 (:*:) subform subform
                                       ]
                 where
                   subform  =  expr (n `div` 2)

-- 3a

rpn :: Expr -> [String]
rpn X = ["X"]
rpn (Const x) = [show x]
rpn (Neg x) = (rpn x) ++ ["-"]
rpn (x :*: y) = (rpn x) ++ (rpn y) ++ ["*"]
rpn (x :+: y) = (rpn x) ++ (rpn y) ++ ["+"]



-- 3 b

evalrpn :: [String] -> Int -> Int
evalrpn = undefined
