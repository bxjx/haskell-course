module Exercise3 where
import Data.List
import Data.Maybe

-- Aim is to make as short as possible

-- first go
skips :: [a] -> [[a]]
skips l = map snd $ map (\n -> unzip (filter (\(i, _) -> i `mod` n == 0) (zip [1..] l) )) [1..(length l)]

-- second go with list comprehensions
skips2 :: [a] -> [[a]]
skips2 l = map (\n -> [ x | (i, x) <- zip [1..] l, i `mod` n == 0]) [1..(length l)]

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) = (if b > a && b > c then [b] else []) ++ (localMaxima (b:c:xs))
localMaxima _ = []

range :: [Int]
range = [0..9]

numberWithFrequencies :: [Int] -> [(Int, Int)]
numberWithFrequencies = map (\ls -> (head ls, length ls)) . group . sort

listOfFrequencies :: [Int] -> [Int]
listOfFrequencies xs = map (\i -> fromMaybe 0 $ (lookup i) $ numberWithFrequencies xs) range

starOrNot :: Int -> Int -> Char
starOrNot currentFrequency frequency = if frequency >= currentFrequency then '*' else ' '

buildRow :: [Int] -> Int -> String
buildRow frequencies row = map (starOrNot row) frequencies

histogram :: [Int] -> String
histogram xs =
  let frequencies = listOfFrequencies xs
      rows = [(maximum frequencies)..1]
  in unlines $ map (buildRow frequencies) rows ++ ["==========", "0123456789"]
