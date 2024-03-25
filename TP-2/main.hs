mapSucesor :: [Int] -> [Int]
mapSucesor [] = []
mapSucesor (x:xs) = (x + 1) : mapSucesor xs

filterPositive :: [Int] -> [Int]
filterPositive [] = []
filterPositive (x:xs) = if x > 0 then x : filterPositive xs else filterPositive xs

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs
mapLen :: [[a]] -> [Int]
mapLen [] = []
mapLen (x:xs) = len x : mapLen xs

main = do 
    print(mapSucesor[3, 4, 6, 8])
    print(filterPositive[2, -4, 5, -8, 7])
    print(mapLen[[2, 7, 5, 4], [3, 2], [], [6, 4, 9]])