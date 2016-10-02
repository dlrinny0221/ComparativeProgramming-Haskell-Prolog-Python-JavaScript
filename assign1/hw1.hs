{-
 - Program: hw1.hs
 - Authors: Yunyi Ding and Brian Lin
 - On this homework, we worked together for 3 hours,
 - Yunyi worked independently for 2 hours,
 - and Brian worked independently for 2 hours.
-}



-- This function takes two Strings: authorFirstName authorLastName
-- and returns a String: first and last names in reversed order
citeAuthor :: String -> String -> String
citeAuthor x y = y ++ ", " ++ x

-- This function takes two Strings: authorFirstName authorLastName
-- and returns a String: the initials of first and last names separated by '.'
initials :: String -> String -> String
initials x y = (take 1 x) ++ "." ++ (take 1 y) ++ "."

initials' :: String -> String -> String
initials' x y = [f] ++ ". " ++ [l] ++ "."
		where (f:_) = x
		      (l:_) = y

-- This function takes a tuple of (author::String, title::String, year::Int)
-- and returns a String: title
title :: (String, String, Int) -> String
title (a, b, c) = b

-- This function takes a tuple of (author::String, title::String, year::Int)
-- and returns a String: author
author :: (String, String, Int) -> String
author (a, b, c) = a

-- This function takes a tuple of (author::String, title::String, year::Int)
-- and returns a String in the format: title(author, year)
citeBook' (a, b, c) = title (a, b, c) ++ " (" ++ author (a, b, c) ++ ", " ++ show c ++ ")"
citeBook (a, b, c) = b ++ " (" ++ a ++ ", " ++ show c ++ ")"

-- This function uses recursion; it takes a list of tuples of (author::String, title::String, year::Int)
-- and returns a String containing all the books as citations in the form returned by 
-- title(author, year), separated by newlines
bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec [] = ""
bibliography_rec ((a, b, c) : xs) = b ++ " (" ++ a ++ ", " ++ show c ++ ") \n" ++ bibliography_rec xs
--bibliography_rec :: [(String, String, Int)] -> IO()
--bibliography_rec xs = putStr (bibliography_rec' xs)

-- This function has the same action as bibliography_rec above
bibliography_fold :: [(String, String, Int)] -> String
bibliography_fold xs = foldl (\acc (a, b, c) -> acc ++ (b ++ " (" ++ a ++ ", " ++ show c ++ ")\n" )) "" xs

-- year is a helper function of averageYear
-- year takes a tuple of (author::String, title::String, year::Int) and returns a Int
-- averageYear takes a list of tuples of (author::String, title::String, year::Int)
-- and returns a Int: the average publication year of the provided books
-- averageYear = sum of years / numBooks
year :: (String, String, Int) -> Int
year (a, b, c) = c
averageYear :: [(String, String, Int)] -> Int
averageYear xs = sum (map year xs) `div` length xs

-- This function takes a String, for example, txt which has references in the format [n]
-- and returns a Int: the total number of references
references :: String -> Int
references t = length (filter (`elem` ["[0]","[1]","[2]","[3]","[4]","[5]","[6]","[7]","[8]","[9]"]) (words t))

{-txt :: String
txt = "[1] and [2] both feature characters who will do whatever it takes to get to their goal, and in the end the thing they want the most ends up destroying them.  In case of [2] this is a whale..."
      -}
--citeBook [("F. Scott Fitzgerald", "The Great Gatsby", 1925), ("Herman Melville", "Moby Dick", 1851)] txt

-- citeConv is a helper function
-- citeConv takes a a list of tuples of (author::String, title::String, year::Int)
-- and a String in the format of "[n]" n is a one digit Int
-- and returns a String: 
citeConv :: [(String, String, Int)] -> String -> String
citeConv xs n = if (n `elem` ["[0]","[1]","[2]","[3]","[4]","[5]","[6]","[7]","[8]","[9]"]) then citeBook(xs!!((read n!!0) - 1)) else n;
citeText :: [(String, String, Int)] -> String -> String
citeText xs t = unwords(map (citeConv xs) (words t))