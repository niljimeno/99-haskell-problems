main :: IO ()
main = do
    let argList = [1::Integer, 2, 3, 4]
    putStrLn "problem 1"
    print $ myLast argList

    putStrLn "problem 2"
    print $ myButLast argList

    putStrLn "problem 3"
    print $ elementAt argList 3

    putStrLn "problem 4"
    print $ myLength argList

    putStrLn "problem 5"
    print $ myReverse argList

    putStrLn "problem 6"
    print $ isPalindrome argList
    print $ isPalindrome "helloolleh"

    putStrLn "problem 7"
    print $ flatten (List [Elem (1::Integer), List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

    let argRepeat = [3::Integer, 3, 3, 4, 5, 6, 6, 7]
    putStrLn "problem 8"
    print $ compress argRepeat

    putStrLn "problem 9"
    print $ pack argRepeat

    putStrLn "problem 10"
    print $ encode argRepeat


myLast :: [a] -> a
myLast = head . reverse

myButLast :: [a] -> a
myButLast = last . init

elementAt :: [a] -> Int -> a
elementAt xs = head . (`drop` xs)

myLength :: [a] -> Int
myLength = foldl (\xs _ -> (+1) xs) 0

myReverse :: [a] -> [a]
myReverse = foldl (\xs x -> x:xs) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome = all id . (zipWith (==) <*> reverse)

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List a) = concat $ map flatten a

compress :: Eq a => [a] -> [a]
compress (a:b:xs)
    | a == b = compress (b:xs)
    | otherwise = a : compress (b:xs)
compress [x] = [x]
compress _ = []

pack :: Eq a => [a] -> [[a]]
pack l@(a:_) = takeWhile (==a) l : pack (dropWhile (==a) l)
pack [] = []

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack
