colorLimits = [
    12, --Red
    13, --Green
    14  --Blue
    ]
    
--Take line of text and retrieve number of cubes and color pairs
getKeyValPairs::String -> [(Int, String)]
getKeyValPairs x = [((read (y !! i)), y !! (i+1)) | i <- [2, 4..((length y) - 1)]]
    where y = words $ filter ( /= ';') (filter ( /= ',') x)

--Compare a touple to it's corresponding limit
compareLimit::(Int, String) -> Bool
compareLimit (a,b) = case b of "red" -> a <= (colorLimits !! 0)
                               "green" -> a <= (colorLimits !! 1)
                               "blue" -> a <= (colorLimits !! 2)

--Compare all touples' number of cubes to their respective limits
evaluateGame::String -> Bool
evaluateGame = not . elem False . map compareLimit . getKeyValPairs

--Get list of valid game IDs
getValidIDs::[Bool] -> [Int]
getValidIDs x = [i+1 | i <-[0..(length x) - 1], (x !! i) == True]

--Get minimum number of cubes of given color
getMinColor::String -> [(Int, String)] -> Int
getMinColor str pairs = foldl (\acc (a,b) -> if (b == str) && (a > acc) then a else acc) 0 pairs

--Get list of all minimum numbers of cubes
getAllMinColor::[(Int, String)] -> [Int]
getAllMinColor pairs = [getMinColor color pairs| color <- ["red", "green", "blue"]]

--Evaluate power of minimum set of a game
evaluateMinPower::[Int] -> Int
evaluateMinPower (x:xs) = foldl (\acc a -> acc * a) x xs

evaluateGame'::String -> Int
evaluateGame' str = evaluateMinPower $ getAllMinColor $ getKeyValPairs str

--Puzzle part one solution
partOne = show . sum . getValidIDs . map evaluateGame . lines

--Puzzle part two solution
partTwo = show . sum . map evaluateGame' . lines

main = interact $ partTwo