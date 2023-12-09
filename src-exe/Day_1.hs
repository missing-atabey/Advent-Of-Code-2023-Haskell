module Day_1
  (solution
  ) where

import Data.Char
import Data.Text (pack, unpack, replace)

--Map of number names and name combos to actual numbers
dictNums = [
	 ("eighthree", "83"),
	 ("eightwo", "82"),
	 ("twone", "21"),
	 ("oneight", "18"),
	 ("sevenine", "79"),

	 ("one", "1"),
	 ("two", "2"),
	 ("three", "3"),
	 ("four", "4"),
	 ("five", "5"),
	 ("six", "6"),
	 ("seven", "7"),
	 ("eight", "8"),
	 ("nine", "9")
	 ]

--Replace numbers in words with actual numbers based on their name or potential number name combinations
replaceTextNums::String -> [(String, String)] -> String
replaceTextNums str [] = str
replaceTextNums str ((a,b):xs) = if null xs
		    	       	 then (unpack $ replace (pack a) (pack b) (pack str))
				        else replaceTextNums (unpack $ replace (pack a) (pack b) (pack str)) xs

--Get input as list of strings, replace number names with numbers, filter numbers, get first and last number in list
solution = interact $ show . sum . map (read::String -> Int) . map ((\(x:xs) -> x:[if null xs then x else last xs]) . filter isNumber) . (map ( `replaceTextNums` dictNums))  . lines
