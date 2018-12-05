module Day1 where

import qualified Data.Set as Set

parseLine :: String -> Integer
parseLine s = (read (filter (\x -> x /= '+') s)) :: Integer

inputs :: IO [Integer]
inputs = do  
  lines <- lines <$> readFile "day1-input.txt"
  return $ fmap (\line -> parseLine line) lines

cycledInputs :: IO [Integer]
cycledInputs = cycle <$> inputs

frequencies :: [Integer] -> [Integer]
frequencies xs = scanl (+) 0 xs

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate xs = go xs Set.empty
  where go [] _ = Nothing
        go (x:xs) s = if Set.member x s 
                           then Just x
                           else go xs (Set.insert x s)
                           