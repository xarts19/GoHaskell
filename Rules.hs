module Rules
( GroupMap
, groups
, liberties
, showGroups
, showGroupsWithLib
, getCaptured
, checkSuicide
, checkKo
) where


import Board
import Data.Maybe
import Data.Function
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

type GroupMap = M.Map BoardCoord Int

{-
type BoardMap = M.Map BoardCoord Board.Cell
-- convert board from list of lists to map from coords to color
-- simplifies analisys
toMap :: Board.Board -> BoardMap
toMap board = toMap' M.empty board 0 0
    where toMap' m [] _ _ = m
          toMap' m ([]:lines) i j = toMap' m lines (i+1) 0
          toMap' m ((cell:line):lines) i j = toMap' (M.insert (i, j) cell m) (line:lines) i (j+1)
-}

neighbors :: Int -> Int -> [BoardCoord]
neighbors i j = [(i, j-1), (i-1, j), (i, j+1), (i+1, j)]

groups :: Board -> Cell -> [[BoardCoord]]
groups board color = map (map fst) $ L.groupBy ((==) `on` snd) $ L.sortBy (compare `on` snd) (M.toList groupMap)
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
                | ((board !! i) !! j) /= color             = grmap
                | otherwise = foldl (\acc x -> propagate x gr_id acc) inserted (neighbors i j)
            where inserted = M.insert (i, j) gr_id grmap

showGroups :: [[BoardCoord]] -> String
showGroups = foldl (\acc x -> acc ++ "( " ++ showGroup x ++ " )\n") []

showGroupsWithLib :: Board -> [[BoardCoord]] -> String
showGroupsWithLib board = foldl (\acc x -> acc ++ "( " ++ showGroup x ++ " ) : L = " ++ show (liberties board x) ++ "\n") []

showGroup :: [BoardCoord] -> String
showGroup = foldl (\acc x -> acc ++ " " ++ fromMaybe "Error" (toGoCoord x)) []

liberties :: Board -> [BoardCoord] -> Int
liberties board group = S.size $ liberties' group
    where liberties' []            = S.empty
          liberties' ((i, j):rest) = S.union (liberties' rest) $ S.fromList (filter isEmpty (neighbors i j))
          size = getSize board
          isEmpty (i, j)
                | i < 0 || j < 0 || i >= size || j >= size = False
                | otherwise = ((board !! i) !! j) == Empty

getCaptured :: Board -> Cell -> [BoardCoord]
getCaptured board color = fromMaybe [] $ L.find (\gr -> liberties board gr == 0) (groups board color)

-- assumes that BoardCoord is Empty
-- For move to be suicidal, it needs to create a group with 0 liberties, but
-- liberties should be counted after all captures have happened
checkSuicide :: Board -> Cell -> BoardCoord -> Either String BoardCoord
checkSuicide board color coord
        | liberties newBoard myGroup == 0 = Left "No fucking way... (It would be a suicide)"
        | otherwise = Right coord
    where simBoard = replaceAll board color [coord]
          newBoard = replaceAll simBoard Empty $ getCaptured simBoard (opposite color)
          myGroup = fromMaybe [coord] $ L.find (\sublist -> coord `elem` sublist) (groups newBoard color)


-- Ko rules forbids to revert board state to the previous one
-- it prevents endless loops
checkKo :: Board -> Board -> Cell -> BoardCoord -> Either String BoardCoord
checkKo prevBoard curBoard color = Right

