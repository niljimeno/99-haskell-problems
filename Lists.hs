module Lists where

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



data Encoded a = Multiple Int a | Single a
    deriving (Show)

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map (\x -> case length x > 1 of
        True -> Multiple (length x) (head x)
        False -> Single (head x)
    ) . pack

decodeModified :: Eq a => [Encoded a] -> [a]
decodeModified ((Single a):xs) = a : decodeModified xs
decodeModified ((Multiple t a):xs) =
    (take t . repeat) a ++ decodeModified xs
decodeModified [] = []

dupli :: [a] -> [a]
dupli = concatMap $ take 2 . repeat

repli :: [a] -> Int -> [a]
repli x n = concatMap (take n . repeat) x

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery x n = take (n-1) x ++ dropEvery (drop n x) n

split :: [a] -> Int -> ([a], [a])
split x n = (take n x, drop n x)

slice :: [a] -> Int -> Int -> [a]
slice x a b
 | a > b = op a b x
 | a < b = op b a x
 where op g s = take (g-s) . drop (s)

rotate :: [a] -> Int -> [a]
rotate x n
 | n >= 0 = drop n x ++ take n x
 | otherwise = rev (take (-n)) x ++ rev (drop (-n)) x
    where rev f = reverse . f . reverse

removeAt :: Int -> [a] -> (a, [a])
removeAt n x = (x!!n, take (n-1) x ++ drop n x)

run :: IO ()
run = do
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
    print $ encode "eeeaaaasxlddd"

    putStrLn "problem 11"
    print $ encodeModified "eeeaaaasxlddd"

    putStrLn "problem 12"
    print $ decodeModified $ encodeModified "eeeaaaasxlddd"

    -- how is exercise 13 different from exercise 11?
    -- i don't get it

    putStrLn "problem 14"
    print $ dupli "asdlkjhhaslhaslkgh"

    putStrLn "problem 15"
    print $ repli "asdlkjhhaslhaslkgh" 3

    putStrLn "problem 16"
    print $ dropEvery "abcdefghik" 3

    putStrLn "problem 17"
    print $ split "abcdefghik" 3

    putStrLn "problem 18"
    print $ slice "abcdefghik" 3 7

    putStrLn "problem 19"
    print $ rotate "abcdefgh" 3
    print $ rotate "abcdefgh" (-2)

    putStrLn "problem 20"
    print $ removeAt 2 "abcdefghik"
