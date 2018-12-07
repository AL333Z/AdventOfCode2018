module Day7 where
  
import qualified Data.Set as S
import qualified Data.Map as M

inputs = readFile "day7-input.txt"

parse :: String -> M.Map Char (S.Set Char)
parse input = M.fromListWith S.union $ concat
    [ [(line !! 5, S.empty)
      , (line !! 36
      , S.singleton $ line !! 5)
      ] 
      | line <- lines input
    ]

next :: M.Map Char (S.Set Char) -> Char
next m = found  
  where
    found = minimum shortest
    shortest = go [] Nothing (M.toList m)
      where
        go ks _        []           = ks 
        go ks Nothing  ((k,v):rest) = go (k:ks) (Just (length v)) rest
        go ks (Just u) ((k,v):rest)
          |  (length v) > u         = go ks     (Just u)          rest
          |  (length v) < u         = go [k]    (Just (length v)) rest
          |   otherwise             = go (k:ks) (Just u)          rest

evolve :: M.Map Char (S.Set Char) -> String -> Char ->  (String, M.Map Char (S.Set Char))
evolve curMap curOutput next = (curOutput ++ [next],  M.map updateM (M.delete next curMap))
  where 
    updateM chars = S.filter (/= next) chars
    
part1 :: M.Map Char (S.Set Char) -> String
part1 m = fst $ loop m ""
  where
    loop ::  M.Map Char (S.Set Char) -> String  -> (String, M.Map Char (S.Set Char))
    loop curMap curOutput = if newMap == M.empty 
                            then (newOutput, newMap)
                            else loop newMap newOutput
      where   
       (newOutput, newMap) = evolve curMap curOutput (next curMap)
