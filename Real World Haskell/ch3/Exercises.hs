-- file: ch03/Exercises.hs

import Tree
import AlgebraicVector

import Data.List (sortBy, minimumBy)

--1, 2
listLen :: [a] -> Int
listLen (_:xs) = 1 + listLen xs
listLen [] = 0

--3
listMean xs = (foldl (+) 0 xs)/fromIntegral (length xs)

--4
listPal xs = xs ++ reverse xs
             where reverse (x:xs) = (reverse xs) ++ [x]
                   reverse []     = []

listPal2 (x:xs) = [x] ++ (listPal2 xs) ++ [x]
listPal2 []     = []

--5
isPal xs = xs == reverse xs

--6
sortByLength = sortBy (\x y -> compare (length x) (length y))

--7
intersperse :: a -> [[a]] -> [a]
intersperse sep (x:[]) = x
intersperse sep (x:xs) = x ++ (sep : (intersperse sep xs))

--8
--treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node x l r) = maximum [1 + treeHeight l, 1 + treeHeight r]

--9
data Direction = DirLeft | DirRight | DirStraight
    deriving (Show)

--10
turnType :: [Cartesian2D] -> Direction
turnType [a, b, c]
    | dir > 0 = DirLeft
    | dir < 0 = DirRight
    | otherwise = DirStraight
    where dir = crossProduct (vector a b) (vector a c)

vector a b = Cartesian2D ((xPos b) - (xPos a)) ((yPos b) - (yPos a))

--angle x y = acos $ (dotProduct x y)/((magnitude x)*(magnitude y))

magnitude (Cartesian2D a b) = sqrt (a*a + b*b)

dotProduct (Cartesian2D a b) (Cartesian2D c d) = sqrt (a*c + b*d)

crossProduct (Cartesian2D a b) (Cartesian2D c d) = a*d - b*c

--11
navigate (a:b:c:xs) = (turnType [a, b, c]):(navigate xs)
navigate _ = []

--12
--grahamScan [] = []
--grahamScan xs = 
--    where minY = minimumBy minYPoint xs

compareByY (Cartesian2D x1 y1) (Cartesian2D x2 y2)
    | (compY == EQ) = compX
    | otherwise = compY
    where compY = compare y1 y2
          compX = compare x1 x2

--Superseded by using minimumBy function
minYPointFrom :: [Cartesian2D] -> Cartesian2D
minYPointFrom (x:xs) = foldl minYPoint x xs

minYPoint a@(Cartesian2D x1 y1) b@(Cartesian2D x2 y2) =
    if y1 == y2 then
        (if x1 <= x2 then a else b)
    else if y1 < y2 then a else b
