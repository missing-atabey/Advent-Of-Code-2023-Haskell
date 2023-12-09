module Day_4 where

import Data.List.Split

getLists::String -> [String]
getLists str = splitOn "|" $ filter (/= '\n') $ drop 1 $ dropWhile (/=':') str

main :: IO ()
main = interact $ (++"\n") . show . getLists
