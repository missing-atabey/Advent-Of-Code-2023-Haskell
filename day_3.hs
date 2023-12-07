import Data.Char
import Data.List (concat)
import Data.Text (pack, unpack, replace)

--Get locations of numbers
findNumIndexes::[String] -> [(Int, Int)]
findNumIndexes strings = [(a,b) | a <- [0..(length strings) - 1], b <- [0..(length (strings!!a)) - 1], isNumber (strings!!a!!b)]

--Get locations of symbols
findSymbolIndexes::[String] -> [(Int , Int)]
findSymbolIndexes strings = [(a,b) | a <- [0..(length strings) - 1], b <- [0..(length (strings!!a)) - 1], (not (isAlphaNum (strings!!a!!b))) && ((strings!!a!!b) /= '.')]

--Get values adjacent to input
getNearby::(Int, Int) -> [(Int, Int)]
getNearby (x,y) = [(x+dx , y+dy) | dx <- [-1..1], dy <- [-1..1], (dx , dy) /= (0 , 0)]

--Validate single digit
isValidDigit::(Int, Int) -> [(Int, Int)] -> Bool
isValidDigit dgt symbols = foldl (\acc x -> x `elem` getNearby dgt || acc) False symbols

main :: IO ()
main = do
  contents <- getContents
  let textLines = lines contents

  --Get numbers in a list
  let nums = words $ concat $ map unpack $ map (replace (pack ".") (pack " ") . pack)$ map (filter (\x -> (isNumber x) || ((isPunctuation x) && x == '.'))) textLines

  --Get index of all digits and symbols
  let numsID = findNumIndexes textLines
  let symsID = findSymbolIndexes textLines

  putStrLn $ show $ nums


{-
Note to self:

To ID valid numbers instead of digits, combine
all digits' valid coords and then validate against
that with the symbols.

To do this group digit coords if they are just 1
away from each other on the y coord (the column).
This way we get as many bools as we have
possible part numbers

maybe use a recursive function and pattern matching like x:xs
to little by little end up with a list of type [[(Int, Int)]]
that we can map a foldl to so we can collapse the nested list
into a bool in order to determine if the number the nested
list corresponds to is valid or not.
-}
