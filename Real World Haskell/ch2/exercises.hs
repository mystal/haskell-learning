
lastButOne :: [a] -> a
lastButOne (x:_:[]) = x
lastButOne (x:xs) = lastButOne xs
lastButOne _ = error "List too short!"
