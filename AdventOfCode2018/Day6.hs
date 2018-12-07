module Day6 where

import Control.Arrow ((&&&))
import qualified Data.HashMap.Strict as M
import Data.Ix (range)
import Data.List.Split (splitOn)

type Coord = (Int, Int)

--TODO do this by myself

dist :: Coord -> Coord -> Int
dist (a, b) (c, d) = abs (a - c) + abs (b - d)

parseCoords :: String -> [Coord]
parseCoords = map (f . splitOn ", ") . lines
    where f [a, b] = (read a, read b)
          f _ = error "Error parsing coord"

