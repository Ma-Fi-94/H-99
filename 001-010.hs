import Control.Arrow ((&&&))

-- Question 1
myLast :: [a] -> a
myLast [] = error "myLast: Empty list."
myLast xs = head . reverse $ xs


-- Question 2
myButLast :: [a] -> a
myButLast xs
    | length xs < 2 = error "myButLast: List too short."
    | otherwise     = head . tail . reverse $ xs


-- Question 3
elementAt :: [a] -> Int -> a
elementAt xs i
    | i > length xs = error "elementAt: Index out of bounds."
    | otherwise     = xs !! (i - 1)


-- Question 4: Tail recursion FTW!
myLength :: [a] -> Int
myLength = go 0
  where
    go acc []     = acc
    go acc (x:xs) = go (acc + 1) xs


-- Question 5: As before
myReverse :: [a] -> [a]
myReverse = go []
  where
    go acc [] = acc
    go acc xs = go (acc ++ [last xs]) (init xs)


-- Question 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs


-- Question 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List []) = []
flatten (List xs) = concatMap flatten xs


-- Question 8
compress :: Eq a => [a] -> [a]
compress []     = []
compress (x:xs) = x : (compress xs')
  where
    xs' = dropWhile (==x) xs


-- Question 9
pack :: Eq a => [a] -> [[a]]
pack []     = []
pack (x:xs) = l : (pack r)
  where
    (l, r) = span (==x) (x:xs)


-- Fanout, my beloved <3
-- Question 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (length &&& head) $ pack xs


main = do
    print $ "Question 001: " ++ show (myLast [1,2,3,4] == 4)
    print $ "Question 001: " ++ show (myLast ['x', 'y', 'z'] == 'z')

    print $ "Question 002: " ++ show (myButLast [1, 2, 3, 4] == 3)
    print $ "Question 002: " ++ show (myButLast ['a'..'z'] == 'y')

    print $ "Question 003: " ++ show (elementAt [1, 2, 3] 2 == 2)
    print $ "Question 003: " ++ show (elementAt "haskell" 5 == 'e')

    print $ "Question 004: " ++ show (myLength [123, 456, 789] == 3)
    print $ "Question 004: " ++ show (myLength "Hello, world!" == 13)

    print $ "Question 005: " ++ show (myReverse "A man, a plan, a canal, panama!" == "!amanap ,lanac a ,nalp a ,nam A")
    print $ "Question 005: " ++ show (myReverse [1, 2, 3, 4] == [4, 3, 2, 1])
    
    print $ "Question 006: " ++ show (isPalindrome [1, 2, 3] == False)
    print $ "Question 006: " ++ show (isPalindrome "madamimadam" == True)
    print $ "Question 006: " ++ show (isPalindrome [1,2,4,8,16,8,4,2,1] == True)

    print $ "Question 007: " ++ show (flatten (Elem 5) == [5])
    print $ "Question 007: " ++ show (null (flatten (List [])))
    print $ "Question 007: " ++ show (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1..5])

    print $ "Question 008: " ++ show (compress "aaaabccaadeeee" == "abcade")

    print $ "Question 009: " ++ show (pack "aaaabccaadeeee" == ["aaaa","b","cc","aa","d","eeee"])

    print $ "Question 010: " ++ show (encode "aaaabccaadeeee" == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')])

