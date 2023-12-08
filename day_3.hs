import Data.Char
import Data.List (concat, groupBy)
import Data.Text (pack, unpack, replace)

--Get locations of numbers
findNumIndexes::[String] -> [(Int, Int)]
findNumIndexes strings = [(a,b) | a <- [0..(length strings) - 1], b <- [0..(length (strings!!a)) - 1], isNumber (strings!!a!!b)]

--Get locations of symbols
findSymbolIndexes::[String] -> [(Int , Int)]
findSymbolIndexes strings = [(a,b) | a <- [0..(length strings) - 1], b <- [0..(length (strings!!a)) - 1], (not (isDigit (strings!!a!!b))) && ((strings!!a!!b) /= '.')]

--Get values adjacent to input
getNearby::(Int, Int) -> [(Int, Int)]
getNearby (x,y) = [(x+dx , y+dy) | dx <- [-1..1], dy <- [-1..1], (dx , dy) /= (0 , 0)]

--Validate single digit
isValidDigit::(Int, Int) -> [(Int, Int)] -> Bool
isValidDigit dgt symbols = foldl (\acc x -> x `elem` getNearby dgt || acc) False symbols

--Get numbers in a list
getNums::[String] -> [Int]
getNums strs = map (read::String -> Int) $ concat $ map words $ map (map (\x -> if not (isDigit x) then ' ' else x)) $ map (filter (\x -> (isNumber x) || ((isPunctuation x)))) strs

--Get numbers in a list of text
getNums'::[String] -> [String]
getNums' strs = concat $ map words $ map (map (\x -> if not (isDigit x) then ' ' else x)) strs

--Group digits that are together
groupDigits :: [(Int, Int)] -> [(Int, Int)]
groupDigits [] = []
groupDigits [x] = [x]
groupDigits ((x1, y1):(x2, y2):xs)
  | x1 == x2 && y1 + 1 == y2 = (x1, y1) : groupDigits ((x2, y2):xs)
  | otherwise = [(x1, y1)]

--Group All numbers
multiGroup::[(Int, Int)] -> [[(Int,Int)]]
multiGroup [] = []
multiGroup [x] = [[x]]
multiGroup x = pairGroup : multiGroup (drop (length pairGroup) x)
  where pairGroup = groupDigits x

--Check if any element in list1 is in list 2
anyTupleInList::[(Int, Int)] -> [(Int,Int)] -> Bool
anyTupleInList list1 list2 = any (`elem` list2) list1

--Check if number is valid [(digits)] -> [()]
isValidNumber::[(Int, Int)] -> [(Int,Int)] -> Bool
isValidNumber digits symbols  = anyTupleInList symbols $ concat allNearby
  where allNearby = map getNearby digits

--Filter valid nums
filterValidNums::[Int] -> [Bool] -> [Int]
filterValidNums numbers bools = [ a | (a,b) <- validityPairs, b == True]
  where validityPairs = zip numbers bools

main :: IO ()
main = do
  contents <- getContents
  let textLines = lines contents
  
  --Get index of all digits and symbols
  let numsID = findNumIndexes textLines
  let symsID = findSymbolIndexes textLines
  
  let numbers = getNums textLines
  let validityBools =  map (`isValidNumber` symsID) $  multiGroup $ numsID

  putStrLn $ show $ sum $ filterValidNums numbers validityBools
