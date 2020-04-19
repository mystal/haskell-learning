-- file: ch04/Exercises.hs
--import InteractWith (interactWith)

import Data.Char (digitToInt)
import Data.List (foldl')

--First set of Exercises

--1
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
--safeLast :: [a] -> Maybe a
--safeInit :: [a] -> Maybe [a]

safeHead (x:xs) = Just x
safeHead _ = Nothing

safeTail (x:xs) = Just xs
safeTail _ = Nothing

safeLast [x] = Just x
safeLast (x:xs) = safeLast xs
safeLast _ = Nothing

--safeInit [x] = Just []
--safeInit (x:xs) = Just (x:(safeInit xs))
--safeInit _ = Nothing

--Smart solution
-- safeListFunc func [] = Nothing
-- safeListFunc func xs = Just (func xs)
--
-- safeHead = safeListFunc head
-- safeTail = safeListFunc tail
-- safeLast = safeListFunc last
-- safeInit = safeListFunc init

--2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = if null b then
                     a:[]
                 else
                     a:(splitWith f (tail b))
    where (a, b) = span f xs

--Second set of Exercises

asInt_fold :: String -> Int
asInt_fold ('-':xs) = negate (asInt_fold xs)
asInt_fold xs = foldl' step 0 xs
    where step acc x = (acc * 10 + digitToInt x)

myConcat :: [[a]] -> [a]
myConcat xs = foldr step [] xs
    where step acc ys = foldr (:) ys acc

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile func (x:xs)
    | func x    = x : myTakeWhile func xs
    | otherwise = []

betterTakeWhile func xs = foldr step [] xs
    where step x acc | func x    = x : acc
                     | otherwise = []

--myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]

myAny :: (a -> Bool) -> [a] -> Bool
myAny func xs = foldr step False xs
    where step x acc | func x    = True || acc
                     | otherwise = False || acc

smartAny pred = foldr (\a b -> pred a || b) False

myCycle :: [a] -> [a]
myCycle [] = error "empty list"
myCycle xs = foldr step [] [xs]
    where step [] _       = foldr step [] [xs]
          step (y:ys) acc = y : (step ys acc)

--myWords :: String -> [String]
--myUnlines :: [String] -> String
