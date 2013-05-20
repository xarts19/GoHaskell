module Rules
( GroupMap
, groups
, showGroups
, checkSuicide
, checkKo
) where


import Board
import Data.Maybe
import Data.Function
import Data.List
import qualified Data.Map as M

type BoardMap = M.Map BoardCoord Board.Cell
type GroupMap = M.Map BoardCoord Int

{-
-- convert board from list of lists to map from coords to color
-- simplifies analisys
toMap :: Board.Board -> BoardMap
toMap board = toMap' M.empty board 0 0
    where toMap' m [] _ _ = m
          toMap' m ([]:lines) i j = toMap' m lines (i+1) 0
          toMap' m ((cell:line):lines) i j = toMap' (M.insert (i, j) cell m) (line:lines) i (j+1)
-}

groups :: Board -> Cell -> [[BoardCoord]]
groups board color = map (map fst) $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) (M.toList groupMap)
    where groupMap = groups' board (0, 0) M.empty 0

          size = getSize board

          groups' :: Board -> BoardCoord -> GroupMap -> Int -> GroupMap
          groups' [] _ grmap _ = grmap
          groups' ([]:ls) (i, _) grmap gr_id = groups' ls (i+1, 0) grmap gr_id
          groups' ((x:l):ls) (i, j) grmap gr_id = groups' (l:ls) (i, j+1) (if x == color then propagate (i, j) gr_id grmap else grmap) (gr_id + 1)

          propagate :: BoardCoord -> Int -> GroupMap -> GroupMap
          propagate (i, j) gr_id grmap
                | i < 0 || j < 0 || i >= size || j >= size = grmap
                | M.member (i, j) grmap                    = grmap
                | ((board !! i) !! j) /= color              = grmap
                | otherwise = foldl (\acc x -> propagate x gr_id acc) inserted neighbors
            where inserted = M.insert (i, j) gr_id grmap
                  neighbors = [(i, j-1), (i-1, j), (i, j+1), (i+1, j)]

showGroups :: [[BoardCoord]] -> String
showGroups [] = "\n"
showGroups (gr:grs) = "( " ++ showGroup gr ++ " )\n" ++ showGroups grs
    where showGroup = foldl (\acc x -> acc ++ " " ++ fromMaybe "Error" (toGoCoord x)) []

checkSuicide :: Board -> BoardCoord -> Either String BoardCoord
checkSuicide board = Right

checkKo :: Board -> BoardCoord -> Either String BoardCoord
checkKo board = Right

