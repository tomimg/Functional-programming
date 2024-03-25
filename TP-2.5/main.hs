import Data.List

duplicateList :: (Int -> Int) -> [Int] -> [Int]
duplicateList l [] = []
duplicateList l (x:xs) = l x : duplicateList l xs

evenOrNot :: [Int] -> Bool
evenOrNot [] = True
evenOrNot (x:xs)
     | mod x 2 == 0 = evenOrNot xs
     | otherwise = False

sumSquaredPairs :: [Int] -> Int
sumSquaredPairs [] = 0
sumSquaredPairs (x:xs) 
    | even x = (x * x) + sumSquaredPairs xs
    | otherwise = sumSquaredPairs xs

isPrime :: Int -> Bool
isPrime x = _isPrime x (x - 1) 
    where
        _isPrime :: Int -> Int -> Bool
        _isPrime x 1 = True
        _isPrime 1 y = True
        _isPrime x y | mod x y == 0 = False
                     | otherwise = _isPrime x (y - 1)

prime :: Int -> [Int]
prime n = take n (filter isPrime [1..])

sortLists :: [Int] -> [Int] -> [Int]
sortLists x y = sort (x ++ y)


main :: IO ()
main = do
    putStrLn "Lista duplicada: "
    print(duplicateList (* 2) [3, 5, 7, 9])
    putStrLn "Lista par? (T or F): "
    print(evenOrNot [4, 6, 3])
    putStrLn "Sumatoria de los cuadrados de los numeros pares de la lista:"
    print(sumSquaredPairs [4, 5, 8])
    putStrLn "Primeros 6 numeros primos:"
    print (prime 6)
    putStrLn "Lista ordenada de dos listas desordenadas:"
    print (sortLists [3, -8, 2, 1, 3, 5] [-3 , 6, 0, 7])