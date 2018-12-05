{-# LANGUAGE FlexibleContexts #-}
module Day4 where

import Text.Parsec
import Text.Parsec.String
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split
import Data.Time
import qualified Data.List as List

inputs :: IO [Entry]
inputs = do  
  lines <- lines <$> readFile "day4-input.txt"
  case sequence $ fmap parseEntry lines of
    Left e -> error $ show e
    Right res -> return res
    
data Shift = Begin | End deriving (Show, Eq, Ord)
data Entry = 
      Shift {time :: UTCTime, guardId :: Int}   
    | Turn { time :: UTCTime }
    deriving (Show, Eq)
    
instance Ord Entry where
  (Shift t1 _) >= (Shift t2 _) = t1 >= t2
  (Shift t1 _) >= (Turn t2) = t1 >= t2
  (Turn t1) >= (Shift t2 _) = t1 >= t2
  (Turn t1) >= (Turn t2) = t1 >= t2
  
  (Shift t1 _) <= (Shift t2 _) = t1 <= t2
  (Shift t1 _) <= (Turn t2) = t1 <= t2
  (Turn t1) <= (Shift t2 _) = t1 <= t2
  (Turn t1) <= (Turn t2) = t1 <= t2

shiftParser = do
  dateStr <- char '[' *> manyTill anyChar (char ']')
  let date = parseTimestamp dateStr
  string " Guard #"
  guardId <- nat
  char ' '
  many1 anyChar
  return $ Shift date guardId
  
asleepParser = do
  dateStr <- char '[' *> manyTill anyChar (char ']')
  let date = parseTimestamp dateStr
  char ' '
  act <- many1 anyChar
  return $ Turn date

parseTimestamp :: String -> UTCTime
parseTimestamp s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M" s
  
-- TODO actually understand why
parser = try shiftParser <|> asleepParser

nat :: Parser Int
nat = do ds <- many1 digit
         return $ read ds
         
parseEntry :: String -> Either ParseError Entry
parseEntry s = parse parser "" s

sortedInputs :: IO [Entry]
sortedInputs = List.sort <$> inputs

partitionEntries :: [Entry] -> Map Int [Entry]
partitionEntries ((Shift _ gId):xs) = goEntries gId xs M.empty
  where
    goEntries :: Int -> [Entry] -> Map Int [Entry] -> Map Int [Entry]
    goEntries gId ((Turn t):[]) acc = updateState gId acc (Turn t)
    goEntries _ ((Shift _ newId):es) acc = goEntries newId es acc
    goEntries gId ((Turn t):es) acc = goEntries gId es (updateState gId acc (Turn t))
    
    updateState gId acc entry = M.insertWith (\acc a -> a ++ acc) gId [entry] acc

sleptMinutes :: Map Int [Entry] -> Map Int [Int]
sleptMinutes m = fmap computeSleptMinutes m
  where
    computeSleptMinutes entries = Data.List.Split.chunksOf 2 entries >>= (\sleep -> minutesAsleep (time $ sleep List.!! 0) (time $ sleep List.!! 1))
    
    minutesAsleep :: UTCTime -> UTCTime -> [Int]
    minutesAsleep asleep awake = [asleepMin..awakeMin-1]
      where 
        asleepMin = (todMin (timeToTimeOfDay (utctDayTime asleep)))
        awakeMin = (todMin (timeToTimeOfDay (utctDayTime awake)))

-- Map id (minutes, total)
totals :: Map Int [Int] ->  Map Int ([Int], Int)
totals m = fmap (\x -> (x, length x)) m

-- (id, (minutes, total))
best :: Map Int ([Int], Int) -> (Int, ([Int], Int))
best m = (found, m M.! found)
  where
    found = (getMaxFromMap m) List.!! 0

    getMaxFromMap m = go [] Nothing (M.toList m)
      where
        go ks _        []           = ks 
        go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
        go ks (Just u) ((k,v):rest)
            | (snd v) < (snd u)     = go ks     (Just u) rest
            | (snd v) > (snd u)     = go [k]    (Just v) rest
            | otherwise = go (k:ks) (Just v) rest

part1 :: (Int, ([Int], Int)) -> Int
part1 (id, (xs, _)) = id * mostFreq
  where
    mostFreq = mostCommon xs

    mostCommon :: Ord a => [a] -> a
    mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . List.group . List.sort


