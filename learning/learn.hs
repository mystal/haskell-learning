import Data.List(foldl')
import qualified Data.IntMap as IM
import qualified Data.Map as M

--Naren's problems

distance :: (Floating a) => [a] -> [a] -> a
distance a b = sqrt . foldl' (+) 0 . map (^2) $ zipWith (-) a b

pairSum :: (Num a) => [(a,a)] -> [a]
pairSum = map (uncurry (+))

buildMap :: (Num a) => [(Int,a)] -> IM.IntMap a
buildMap = foldl' (\m (k,v) -> IM.insertWith (+) k v m) IM.empty

eqClasses :: (a -> a -> Bool) -> [a] -> [[a]]
eqClasses f xs = foldl' classify [] xs
    where classify [] x = [[x]]
          classify (c@(y:ys):cs) x 
            | f y x = (x:c):cs
            | otherwise = c : (classify cs x)

--Naren's implementation using `partition`
{-eqClasses :: (a -> a -> Bool) -> [a] -> [[a]]
eqClasses r xs = part xs []
    where part (a:as) acc = let (p1,p2) = partition (r a) as
        in part p2 = ((a:p1):acc)
           part [] acc = acc-}

count :: (Eq a) => [a] -> a -> Int
count xs y = foldl' (\a x -> if (x == y) then a + 1 else a) 0 xs

--apply :: (Ord a) => (a -> b) -> [a] -> M.Map a b
--apply f = foldl' (\m x -> M.insert x (f x) m) M.empty

apply :: (Ord a) => (a -> b) -> [a] -> a -> b
apply f xs y = (foldl' (\m x -> M.insert x (f x) m) M.empty xs) M.! y

findAndAdd :: M.Map Int Int -> Int -> Maybe Int
findAndAdd m x = fmap (+100) (M.lookup x m)
