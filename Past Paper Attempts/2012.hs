-- Informatics 1 Functional Programming
-- December 2012
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck,
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )
import Control.Monad -- defines liftM, liftM2, used below

main :: IO ()
main = return ()

-- Question 1

-- 1a

replaceDigit :: Int -> Bool
replaceDigit i = i `mod` 2 == 0

f :: Int -> [Int] -> [Int]
f a xs = [if replaceDigit i then a else x | (x,i) <- zip xs [0..]]

-- 1b

g :: Int -> [Int] -> [Int]
g a xs = g' a xs 0
  where
    g' :: Int -> [Int] -> Int -> [Int]
    g' b [] i = []
    g' b (y:ys) i | replaceDigit i = b : (g' b ys (i+1))
                  | otherwise = y : (g' b ys (i+1))

-- Question 2

-- 2a

p :: [Int] -> Bool
p xs = and[even x | x <- xs, x >= 10 && x <= 100]

-- 2b

q :: [Int] -> Bool
q [] = True
q (x:xs) | x >= 10 && x <= 100 = if even x then q xs else False
         | otherwise = q xs

-- 2c

r :: [Int] -> Bool
r xs = foldr (&&) True (map (even) (filter (<= 100) (filter (>= 10) xs)))

-- Question 3

data Prop = X
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          deriving (Eq, Ord)

-- turns a Prop into a string approximating mathematical notation

showProp :: Prop -> String
showProp X          =  "X"
showProp F          =  "F"
showProp T          =  "T"
showProp (Not p)    =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)  =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"

-- For QuickCheck

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       ]
                 where
                   atom = oneof [elements [X,F,T]]
                   subform  =  prop (n `div` 2)

-- 3a

eval :: Prop -> Bool -> Bool
eval T _ = True
eval F _ = False
eval X v = v
eval (Not p) v = not (eval p v)
eval (p :|: q) v = eval p || eval q


-- 3b

simplify :: Prop -> Prop
simplify =  undefined
