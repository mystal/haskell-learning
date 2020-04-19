correlation :: (Floating a) => a -> a -> a -> a
correlation cov sdx sdy = let d = sdx*sdy
                          in if d == 0 then 0 else cov/d

covariance :: (Floating a) => a -> a -> [a] -> [a] -> a
covariance ux uy xs ys = mean $ zipWith (\x y -> (x-ux)*(y-uy)) xs ys

mean :: (Floating a) => [a] -> a
mean [] = 0
mean xs = uncurry (/).foldl' (\(total,count) v ->
                              let t = total+v
                                  c = count+1
                              in t `seq` c `seq` (total+v,count+1)) (0,0) $ xs

variance :: (Floating a) => a -> [a] -> a
variance ux xs = (mean.map (^2)) xs - ux^2

std :: (Floating a) => a -> [a] -> a
std ux xs = sqrt $ variance ux xs


chisquared :: (Floating a) => Int -> ((a,a) -> Double) -> Double -> [a] -> Bool
