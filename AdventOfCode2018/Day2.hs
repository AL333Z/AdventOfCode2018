module Day2 where

import qualified Data.List as List

inputs :: IO [String]
inputs = lines <$> readFile "day2-input.txt"

--TODO Ior?
data Occurence = None | Two | Three | Both deriving (Show, Eq)

distinctLetters :: String -> [Char]
distinctLetters s = List.nub s

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

containsAtLeastANuple :: Int -> String -> Bool
containsAtLeastANuple n s = go (distinctLetters s) s
  where 
    go [] _ = False
    go (x:xs) s = if (count x s) == n
                    then True 
                    else go xs s

compute :: String -> Occurence
compute s = occurr two three
  where
    two = containsAtLeastANuple 2 s
    
    three = containsAtLeastANuple 3 s
    
    occurr True True = Both
    occurr True False = Two
    occurr False True = Three
    occurr _ _ = None

occurences :: [String] -> [Occurence]
occurences = fmap compute

checksum :: [Occurence] -> Int
checksum xs = twos * threes
  where
    both = count Both xs
    twos = both + (count Two xs)
    threes = both + (count Three xs)

diff :: String -> String -> Int
diff [] [] = 0
diff (x:xs) (y:ys) = if x == y 
                     then diff xs ys
                     else 1 + diff xs ys
                            
computeDiffs :: [String] -> String  -> [(String, String, Int)]
computeDiffs xs s = [ (s, x, diff s x) | x <- xs]

allDiffs :: [String] -> [(String, String, Int)]
allDiffs xs = reverse $ [ res | x <- xs, res <- computeDiffs xs x]

onlyOneDiff (_, _, 1) = True
onlyOneDiff _ = False

theId :: [(String, String, Int)] -> Maybe String
theId xs = removeDiff `fmap` List.find onlyOneDiff xs  
  where 
    removeDiff (a, b, _) = a `List.intersect` b
