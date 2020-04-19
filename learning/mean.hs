import Data.List(foldl')

meanl :: (Floating a) => [a] -> a
meanl xs = a / b
    where (a,b) = foldl inc (0,0) xs
                  where inc (s,c) x = (s+x,c+1)

meanr :: (Floating a) => [a] -> a
meanr xs = a / b
    where (a,b) = foldr inc (0,0) xs
                  where inc x (s,c) = (s+x,c+1)

meanl' :: (Floating a) => [a] -> a
meanl' xs = a / b
    where (a,b) = foldl' inc (0,0) xs
                  where inc (s,c) x = (s+x,c+1)
