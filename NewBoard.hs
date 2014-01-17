module Board
(
) where

import Control.Applicative ( (<$>), (<*>) )
import Data.Maybe
import Data.Char ( toUpper )
import Data.List ( elemIndex )


data Cell = Empty | White | Black deriving (Eq, Show, Read)
type Coord = (Int, Int)


type Board = Map ImplCoord Cell


opposite :: Cell -> Cell
opposite White = Black
opposite Black = White
opposite Empty = Empty

makeBoard :: Int -> Board
makeBoard size = [[Empty | _ <- [0..size-1]] | _ <- [0..size-1]]

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads

-- Nothing == wrong turn specification; Just (-1, -1) == pass
fromGoCoord :: GoCoord -> Maybe BoardCoord
fromGoCoord "" = Nothing
fromGoCoord (ch:rest)
                | not (fromMaybe False ((&&) <$> fmap bounds i <*> fmap bounds j)) = Nothing
                | otherwise = (,) <$> j <*> i
    where bounds = inBounds 0 19
          i = elemIndex (toUpper ch) letterCoords
          j = (\x -> x - 1) <$> maybeRead rest

inBounds :: Int -> Int -> Int -> Bool
inBounds low high val = low <= val && val < high

toGoCoord :: BoardCoord -> Maybe GoCoord
toGoCoord (i, j)  | i < 0 || j < 0 || i >= 19 || j >= 19 = Nothing
                  | otherwise = Just (letterCoords !! j : show (i+1))

getSize :: Board -> Int
getSize = length

replaceElem :: Board -> Cell -> BoardCoord -> Board
replaceElem board new_elem (i, j) = replace' board (replace' (board !! i) new_elem j) i
    where replace' line cell k = let (xs, _:ys) = splitAt k line in xs ++ (cell : ys)

replace :: Board -> Cell -> [BoardCoord] -> Board
replace board _ [] = board
replace board new_elem (ij:ijs) = replace (replaceElem board new_elem ij) new_elem ijs

checkBounds :: Board -> BoardCoord -> Maybe String
checkBounds board (i, j) | i >= size || j >= size = Just "Coordinates are too big"
                       | otherwise              = Nothing
    where size = getSize board

checkOccupied :: Board -> BoardCoord -> Maybe String
checkOccupied ([]:_) _ = Just "Too large row index"
checkOccupied [] _ = Just "Too large column index"
checkOccupied ((x:_):_) (0, 0)
    | x == Empty = Nothing
    | otherwise = Just "Cell already occupied"
checkOccupied ((_:xs):ys) (0, j) = checkOccupied (xs:ys) (0, j-1)
checkOccupied (_:xs) (i, j) = checkOccupied xs (i-1, j)

showCompactCell :: Cell -> Char
showCompactCell Empty = '+'
showCompactCell White = 'O'
showCompactCell Black = '@'

showCompactRow :: BoardRow -> String
showCompactRow = foldr (\x -> (showCompactCell x :)) ""

showCompact :: Board -> String
showCompact [] = "\n"
showCompact (r:rs) = showCompactRow r ++ "\n" ++ showCompact rs

showFull :: Board -> String
showFull [] = "\n"
showFull (r:rs) = show r ++ "\n" ++ showFull rs

showAnnotated :: Board -> String
showAnnotated board = header ++ showAnnotated' board 1 ++ header
    where size = length board
          header = showAnnotatedHeader size

showAnnotated' :: Board -> Int -> String
showAnnotated' [] _ = ""
showAnnotated' (r:rs) n = showAnnotated' rs (n+1) ++ showRow
    where row_n_str = show n
          fill = replicate (2 - length row_n_str) ' '
          showRow = fill ++ row_n_str ++ "|" ++ showCompactRow r ++ "|" ++ row_n_str ++ fill ++ "\n"

showAnnotatedHeader :: Int -> String
showAnnotatedHeader size = "   " ++ take size letterCoords ++ "   \n"

boardToList :: Board -> [(Int, Int, Cell)]
boardToList board = boardToList' [] board 0 0
    where rowToList result [] _ _ = result
          rowToList result (x:xs) i j = (i, j, x):(rowToList result xs (i+1) j)
          boardToList' result [] _ _ = result
          boardToList' result (r:rs) i j = (rowToList [] r i j) ++ boardToList' result rs i (j+1)


