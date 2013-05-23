module Rules
( GroupMap
, chains
, liberties
, territory
, showChains
, showChainsWithLib
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


type Chain = [BoardCoord]
chains :: Board -> Cell -> [Chain]
chains board color = map (map fst) $ L.groupBy ((==) `on` snd) $ L.sortBy (compare `on` snd) (M.toList groupMap)
    where groupMap = chains' board (0, 0) M.empty 0

          size = getSize board

          chains' :: Board -> BoardCoord -> GroupMap -> Int -> GroupMap
          chains' [] _ grmap _ = grmap
          chains' ([]:ls) (i, _) grmap gr_id = chains' ls (i+1, 0) grmap gr_id
          chains' ((x:l):ls) (i, j) grmap gr_id = chains' (l:ls) (i, j+1) (if x == color then propagate (i, j) gr_id grmap else grmap) (gr_id + 1)

          propagate :: BoardCoord -> Int -> GroupMap -> GroupMap
          propagate (i, j) gr_id grmap
                | i < 0 || j < 0 || i >= size || j >= size = grmap
                | M.member (i, j) grmap                    = grmap
                | ((board !! i) !! j) /= color             = grmap
                | otherwise = foldl (\acc x -> propagate x gr_id acc) inserted (neighbors i j)
            where inserted = M.insert (i, j) gr_id grmap


showChains :: [Chain] -> String
showChains = foldl (\acc x -> acc ++ "( " ++ showChain x ++ " )\n") []


showChainsWithLib :: Board -> [Chain] -> String
showChainsWithLib board = foldl (\acc x -> acc ++ "( " ++ showChain x ++ " ) : L = " ++ show (liberties board x) ++ "\n") []


showChain :: Chain -> String
showChain = foldl (\acc x -> acc ++ " " ++ fromMaybe "Error" (toGoCoord x)) []


liberties :: Board -> Chain -> Int
liberties board group = neighborCount board group Empty


-- Empty return means Neutral territory
territoryColor :: Board -> Chain -> Cell
territoryColor board chain
            | blackCount > 0 && whiteCount == 0 = Black
            | whiteCount > 0 && blackCount == 0 = White
            | otherwise = Empty
    where blackCount = neighborCount board chain Black
          whiteCount = neighborCount board chain White


territory :: Board -> Cell -> Int
territory board color = length $ concat $ filter ((==color) . territoryColor board) (chains board Empty)


type NeighborColor = Cell
neighborCount :: Board -> Chain -> NeighborColor -> Int
neighborCount board group color = S.size $ count' group
    where count' []            = S.empty
          count' ((i, j):rest) = S.union (count' rest) $ S.fromList (filter isOfColor (neighbors i j))
          size = getSize board
          isOfColor (i, j)
                | i < 0 || j < 0 || i >= size || j >= size = False
                | otherwise = ((board !! i) !! j) == color


getCaptured :: Board -> Cell -> [BoardCoord]
getCaptured board color = concat $ filter (\gr -> liberties board gr == 0) (chains board color)


-- assumes that BoardCoord is Empty
-- For move to be suicidal, it needs to create a group with 0 liberties, but
-- liberties should be counted after all captures have happened
checkSuicide :: Board -> Cell -> BoardCoord -> Either String BoardCoord
checkSuicide board color coord
        | liberties newBoard myChain == 0 = Left "Do you value your life at all?"
        | otherwise = Right coord
    where newBoard = simulateMove board color coord
          myChain = fromMaybe [coord] $ L.find (\chain -> coord `elem` chain) (chains newBoard color)


-- Ko rules forbids to revert board state to the previous one
-- it prevents endless loops
checkKo :: Board -> Board -> Cell -> BoardCoord -> Either String BoardCoord
checkKo prevBoard curBoard color coord
        | prevBoard == newBoard = Left "Ko rule: prohibition of repetition"
        | otherwise = Right coord
    where newBoard = simulateMove curBoard color coord


simulateMove :: Board -> Cell -> BoardCoord -> Board
simulateMove board color coord = replace simBoard Empty $ getCaptured simBoard (opposite color)
    where simBoard = replace board color [coord]

