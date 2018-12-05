module Day3 where

import Text.Parsec
import Text.Parsec.String
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split

inputs :: IO [Claim]
inputs = do  
  lines <- lines <$> readFile "day3-input.txt"
  case sequence $ fmap parseClaim lines of
    Left e -> error $ show e
    Right res -> return res

parseClaim :: String -> Either ParseError Claim
parseClaim s = parse parser "" s

data Claim = Claim
    { id :: Int 
    , x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    }
    deriving (Show)

data Point = Point Int Int deriving (Show, Ord, Eq)

parser = do
  char '#'
  claimId <- nat <* space
  char '@' *> space
  l <- nat
  char ','
  t <- nat
  char ':' *> space
  w <- nat 
  char 'x'
  h <- nat
  return $ Claim claimId l t w h 
  
nat :: Parser Int
nat = do ds <- many1 digit
         return $ read ds

points :: Claim -> [Point]
points (Claim _ x y w h) = 
    [ Point (x + dx) (y + dy)
    | dx <- [0..w - 1]
    , dy <- [0..h - 1]
    ]

overlap :: [Claim] -> Set Point
overlap cs = M.keysSet . M.filter (>= 2) $ freq
    where
      freq :: Map Point Int 
      freq = M.fromListWith (+) [(p, 1) | c <- cs, p <- points c ]

hasOverlap :: Set Point -> Claim -> Bool
hasOverlap o = all (`S.notMember` o) . points

notOverlapped :: Set Point -> [Claim] -> Claim
notOverlapped o = head . filter (hasOverlap o)
