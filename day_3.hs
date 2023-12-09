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

--Get list of bools that validates each number's index
getValidNumberIndexes::[String] -> [Bool]
getValidNumberIndexes strs =
  let syms = findSymbolIndexes strs
      nums = findNumIndexes strs
  in map (`isValidNumber` syms) $  multiGroup $ nums

--Find ID of all asterisks
getAllAsterisks::[String] -> [(Int,Int)]
getAllAsterisks strs = [(a,b) | (a,b) <- findSymbolIndexes strs, strs!!a!!b == '*']

--Pair consecutive findNumIndexes
pairConsecutive :: [a] -> [(a, a)]
pairConsecutive [] = []
pairConsecutive (x:y:xs) = (x,y):pairConsecutive xs

--Get Gear Ratios (Ignore how horribly made this is)
getGearRatios::[String] -> [Int]
getGearRatios strs =
  let validGroups =
        let nums = findNumIndexes strs
            groups = multiGroup nums
            validityBools = getValidNumberIndexes strs

        in [groups!!a | a <- [0..length groups - 1], validityBools!!a == True]

      asterisks = getAllAsterisks strs
      asteriskInGroup = map (\asterisk -> map (\group -> any (`elem` group) (getNearby asterisk)) validGroups) asterisks
        
      validAsterisks =
        let counts = map (\x -> length $ filter (==True) x) asteriskInGroup
        in [if counts!!i == 2 then asteriskInGroup!!i else [] | i <- [0..length asteriskInGroup - 1]]
      validGroupIndexes = concat $ filter (not . null)$ map (\bools -> if null bools then [] else filter (>=0) [if bools!!i == True then i else -1 | i <- [0..length bools -1]]) validAsterisks

      nums = filterValidNums (getNums strs) (getValidNumberIndexes strs)
  in map (\(a,b) -> a*b)$ pairConsecutive $  map (\index -> nums!!index) validGroupIndexes

--Solve Part One
partOne::[String] -> Int
partOne strs =
  let
    numbers = getNums strs
    validityBools = getValidNumberIndexes strs
  in sum $ filterValidNums numbers validityBools

--Solve Part Two
partTwo::[String] -> Int
partTwo strs = sum $ getGearRatios $ strs

{-
    ---------------MAIN FUNCTION--------------------
-}

main :: IO ()
main = interact $ ("Results: " ++) . (++ "\n") . show . partTwo . lines
