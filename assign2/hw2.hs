{-
 - Authors: Yunyi Ding and Brian Lin
 - 
-}

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl op a [] = a
myFoldl op a (x:xs) = myFoldl op (op a x) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f a bs = foldl (\g b x -> g (f b x)) id bs a
--myFoldr op n [] = n
--myFoldr op n list = myFoldr op (op (last list) n) (init list)

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f a bs = foldr (\b g x -> g (f x b)) id bs a

isUpper :: Char -> Bool
isUpper c = elem c ['A'..'Z']

onlyCapitals1 :: String -> String
onlyCapitals1 s = filter isUpper s

onlyCapitals2 :: String -> String
onlyCapitals2 s = [x | x <- s, isUpper x ]

onlyCapitals3 :: String -> String
onlyCapitals3 [] = ""
onlyCapitals3 (x:xs)
	| isUpper x = [x] ++ onlyCapitals3 xs
	| otherwise = onlyCapitals3 xs

divRemainder :: Int -> Int -> (Int, Int)
divRemainder a b = (a `div` b, a `mod` b)

digitSum :: Int -> Int
digitSum a
	| a > 9 = read [head (show a)] + digitSum (read (tail (show a)))
	| otherwise = a

sayNum :: String -> String
sayNum "1" = "one"
sayNum "2" = "two"
sayNum "3" = "three"
sayNum "4" = "four"
sayNum "5" = "five"
sayNum "6" = "six"
sayNum "7" = "seven"
sayNum "8" = "eight"
sayNum "9" = "nine"
sayNum "10" = "ten"
sayNum "11" = "eleven"
sayNum "12" = "twelve"
sayNum "13" = "thirteen"
sayNum "14" = "fourteen"
sayNum "15" = "fifteen"
sayNum "16" = "eleven"
sayNum "17" = "twelve"
sayNum "18" = "eighteen"
sayNum "19" = "nineteen"
sayNum "20" = "twenty"
sayNum "30" = "thirty"
sayNum "40" = "forty"
sayNum "50" = "fifty"
sayNum "60" = "sixty"
sayNum "70" = "seventy"
sayNum "80" = "eighty"
sayNum "90" = "ninty"
sayNum s
	| length sayNum > 3 = sayNum(take ((length sayNum) -3) s) ++ (illionName (length sayNum)/3) ++ sayNumH (reverse (take 3 (reverse s)))
	| otherwise sayNum s

illionName :: Int -> String
illionName 0 = ""
illionName 1 = "thousand"
illionName 2 = "millon"
illionName 3 = "billion"
illionName 4 = "trillion"
illionName 5 = "quadrillion"
illionName 6 = "quintillion"
illionName 7 = "sextillion"
illionName 8 = "septillion"
illionName 9 = "octillion"
illionName 10 = "nonillion"
illionName 11 = "decillion"
illionName 12 = "undecillion"
illionName 13 = "duodecillion"
illionName 14 = "tredecillion"
illionName 15 = "quattuordecillion"
illionName 16 = "quindecillion"
illionName 17 = "sexdecillion"
illionName 18 = "deptendecillion"
illionName 19 = "octodecillion"
illionName 20 = "novemdecillion"
illionName 21 = "vigintillion"

sayNumH :: String -> String
sayNumH s = 