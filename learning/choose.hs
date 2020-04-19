
choose1 :: [a] -> [[a]]
choose1 [] = []
choose1 xs = map (\x -> [x]) xs

choose2 :: [a] -> [[a]]
choose2 [] = []
choose2 (x:xs) = map (\y -> [x, y]) xs ++ choose2 xs

choose :: [a] -> Int -> [[a]]
choose [] _ = []
choose (x:xs) n
    | n <= 0    = []
    | n == 1    = [x] : choose xs n
    | otherwise = (map (\y -> x : y) (choose xs (n - 1))) ++ choose xs n
