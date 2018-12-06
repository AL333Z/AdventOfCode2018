module Day5 where
  
import Data.Char
import qualified Data.Set as Set
import qualified Data.List as List
  
inputs = readFile "day5-input.txt"

part1 :: String -> Int
part1 xs = length $ foldr step "" xs
  where
    step x (y:ys) | x /= y && toUpper x == toUpper y = ys
    step x ys = x : ys

part2 :: String -> Int
part2 s = minimum reductions
  where
    letters = distinctLetters s
    combinations = stringsWithoutChar letters s   
    reductions = fmap part1 combinations
    
    strUpper :: String -> String
    strUpper xs = toUpper `fmap` xs
  
    distinctLetters :: String -> [Char]
    distinctLetters = Set.toList . Set.fromList . strUpper

    stringsWithoutChar :: [Char] -> String -> [String]
    stringsWithoutChar chars xs = [ filter (\s -> s /= c && s /= (toLower c)) xs | c <- chars ] 
