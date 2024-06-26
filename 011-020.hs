import Control.Arrow ((&&&))

-- Question 9, for Question 11
pack :: Eq a => [a] -> [[a]]
pack []     = []
pack (x:xs) = l : (pack r)
  where
    (l, r) = span (==x) (x:xs)


-- Question 10, for Question 11
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (length &&& head) $ pack xs


-- Question 11
data EncodedElem a = Single a | Multiple Int a deriving Eq

encodeModified :: Eq a => [a] -> [EncodedElem a]
encodeModified xs = map translateTuple $ encode xs
  where
    translateTuple (1, x) = Single x
    translateTuple (n, x) = Multiple n x

-- Question 12
decodeModified :: [EncodedElem a] -> [a]
decodeModified xs = concatMap decode xs
  where
    decode (Single x)     = [x]
    decode (Multiple n x) = replicate n x


-- Question 13
encodeDirect :: Eq a => [a] -> [EncodedElem a]
encodeDirect []     = []
encodeDirect (x:xs) = cur : encodeDirect rest
  where
    cur   = if count == 1
            then Single x
            else Multiple count x
    rest  = dropWhile (==x) (x:xs)
    count = length $ takeWhile (==x) (x:xs)


-- Question 14: Special case of Question 15 LOL
dupli :: [a] -> [a]
dupli = flip repli 2


-- Question 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs


-- Question 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = (take (n - 1) xs) ++ (dropEvery (drop n xs) n)


-- Question 17: Without any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split xs n = go [] xs 0
  where
    go left right@(r:rs) i
        | i == n    = (left, right)
        | otherwise = go (left ++ [r]) rs (i + 1) 


-- Question 18
slice :: [a] -> Int -> Int -> [a]
slice xs i j
    | i >= 1 && j <= length xs && i <= j = drop (i - 1) $ take j xs
    | otherwise                          = error "slice: OOB."


-- Question 19
rotate :: [a] -> Int -> [a]
rotate xs n = (drop k xs) ++ (take k xs)
  where
    k = n `mod` length xs


-- Question 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs
    | n >= 1 && n <= length xs = (r, xs')
    | otherwise                = error "removeAt: OOB."
  where
    r   = xs !! (n - 1)
    xs' = take (n - 1) xs ++ drop n xs

main = do
    print $ "Question 11: " ++ show (encodeModified "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'])
    print $ "Question 12: " ++ show (decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'] == "aaaabccaadeeee")
    print $ "Question 13: " ++ show (encodeDirect "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'])
    print $ "Question 14: " ++ show (dupli [1, 2, 3] == [1, 1, 2, 2, 3, 3])
    print $ "Question 15: " ++ show (repli "abc" 3 == "aaabbbccc")
    print $ "Question 16: " ++ show (dropEvery "abcdefghik" 3 == "abdeghk")
    print $ "Question 17: " ++ show (split "abcdefghik" 3 == ("abc", "defghik"))
    print $ "Question 18: " ++ show (slice ['a'..'k'] 3 7 == ['c'..'g'])
    print $ "Question 19: " ++ show (rotate ['a'..'h'] 3 == "defghabc")
    print $ "Question 19: " ++ show (rotate ['a'..'h'] (-2) == "ghabcdef")
    print $ "Question 20: " ++ show (removeAt 2 "abcd" == ('b',"acd")q)
    
    
    print $ "---"

