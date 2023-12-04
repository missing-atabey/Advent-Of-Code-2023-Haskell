import Data.Char
import Data.Text (pack, unpack, replace)

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

replaceTextNums::String -> [(String, String)] -> String
replaceTextNums str [] = str
replaceTextNums str ((a,b):xs) = if null xs
		    	       	 then (unpack $ replace (pack a) (pack b) (pack str))
				        else replaceTextNums (unpack $ replace (pack a) (pack b) (pack str)) xs

main = interact $ show . sum . map (read::String -> Int) . map ((\(x:xs) -> x:[if null xs then x else last xs]) . filter isNumber) . (map ( `replaceTextNums` dictNums))  . lines
