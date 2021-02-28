-- Informatics 1 - Functional Programming
-- Tutorial 2
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck
import Data.Tuple

import Data.Function
import Data.Maybe


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | even x = div x 2  : halveEvensRec xs
                     | otherwise = halveEvensRec xs

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvens xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs) | lo <= x, x <= hi = x : inRangeRec lo hi  xs
                        | otherwise = inRangeRec lo hi xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRangeRec lo hi xs == inRange lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | x > 0  = 1 + countPositivesRec xs
                         | otherwise = countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs


-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x = (digitToInt x) * multDigitsRec xs
                     | otherwise = multDigitsRec xs

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp c t = if isAlpha c then head [b | (a,b) <- t, a == (toUpper c)] else c

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec c [] = c
lookUpRec c (x:xs) | fst x == toUpper c = snd x
                   | otherwise = lookUpRec c xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUp c k == lookUpRec c k


-- 6.

checkLetter :: Int -> Int
checkLetter x = if x > 90 then 64 + (x - 90) else x

encipher :: Int -> Char -> Char
encipher o c = if isAlpha c then chr (checkLetter((ord c) + o)) else c

-- 7.
normalise :: String -> String
normalise [] = []
normalise (x:xs) | isAlpha x = x : normalise xs
                 | isDigit x = x : normalise xs
                 | otherwise = normalise xs


-- 8.

encipherStrComp :: Int -> String -> String
encipherStrComp o str = normalise [encipher o x | x <- str]

encipherStr :: Int -> String -> String
encipherStr o [] = []
encipherStr o (x:xs) = normalise (encipher o x : encipherStr o xs)


-- Optional Material
-- =================

-- 9.

--reverseKey :: [(Char, Char)] -> [(Char, Char)]
--reverseKey t = [(b,a) | (a,b) <- t]

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey t = [swap x | x <- t]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec = map swap

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey t = reverseKey t == reverseKeyRec t


-- 10.

dCheckLetter :: Int -> Int
dCheckLetter x = if x < 64 then 90 - (64 - x) else x

decipher :: Int -> Char -> Char
decipher o c =
  if isDigit c || ord c == 3 then c
  else if isUpper c then chr(dCheckLetter((ord c) - o))
  else '#'

decipherStr :: Int -> String -> String
decipherStr o [] = []
decipherStr o (x:xs) | isDigit x = x : decipherStr o xs
                     | ord x == 32 = x : decipherStr o xs
                     | isUpper x = chr(dCheckLetter((ord x) - o)) : decipherStr o xs
                     | otherwise = decipherStr o xs


-- 11.

contains :: String -> String -> Bool
contains x s = isInfixOf s x


-- 12.

candidates :: Int -> String -> [(Int, String)]
candidates n [] = []
candidates n str | (contains (decipherStr n str) "THE") = (n, (decipherStr n str)) : (candidates (n+1) str)
                 | (contains (decipherStr n str) "AND") = (n, (decipherStr n str)) : (candidates (n+1) str)
                 | otherwise = (candidates (n+1) str)

-- 13.

--splitEachFive :: Int -> String -> [String]
--splitEachFive n (x:xs) | n < 6 = (splitEachFive n+1 xs) !! n


-- 14.

prop_transpose :: String -> Bool
prop_transpose = undefined


-- 15.

encrypt :: Int -> String -> String
encrypt = undefined


-- 16.

decrypt :: Int -> String -> String
decrypt = undefined
