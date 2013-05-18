module Board
( Cell(..)
, opposite
, Board
, BoardType(..)
, makeBoard
, getSize
, replace
, replaceAll
, showCompact
, showFull
, showAnnotated
, letterCoords
) where

data Cell = Empty | White | Black deriving (Eq, Show)
type BoardRow = [Cell]
type Board = [BoardRow]
data BoardType = Small | Medium | Large deriving (Eq)

letterCoords :: String
letterCoords = "ABCDEFGHJKLMNOPQRST"

opposite :: Cell -> Cell
opposite White = Black
opposite Black = White
opposite Empty = Empty

boardSize :: BoardType -> Int
boardSize Small = 9
boardSize Medium = 13
boardSize Large = 19

instance Show BoardType where
    show Small = "Small (9)"
    show Medium = "Medium (13)"
    show Large = "Large (19)"

makeBoard :: BoardType -> Board
makeBoard boardType = makeBoard' $ boardSize boardType
    where
        makeBoard' size = [[Empty | _ <- [0..size-1]] | _ <- [0..size-1]]

getSize :: Board -> Int
getSize = length

replace :: Board -> Cell -> Int -> Int -> Board
replace board new_elem i j = replace' board (replace' (board !! correct_i) new_elem j) correct_i
    where correct_i = getSize board - i - 1
          replace' line cell k = let (xs, _:ys) = splitAt k line in xs ++ (cell : ys)

replaceAll :: Board -> Cell -> [(Int, Int)] -> Board
replaceAll board _ [] = board
replaceAll board new_elem ((i, j):ijs) = replaceAll (replace board new_elem i j) new_elem ijs

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

showAnnotatedHeader :: Int -> String
showAnnotatedHeader size = "   " ++ take size letterCoords ++ "   \n"

showAnnotated' :: Board -> Int -> String
showAnnotated' [] _ = ""
showAnnotated' (r:rs) n = showRow ++ showAnnotated' rs (n-1)
    where row_n_str = show n
          fill = replicate (2 - length row_n_str) ' '
          showRow = fill ++ row_n_str ++ "|" ++ showCompactRow r ++ "|" ++ row_n_str ++ fill ++ "\n"

showAnnotated :: Board -> String
showAnnotated board = header ++ showAnnotated' board size ++ header
    where size = length board
          header = showAnnotatedHeader size

