import Data.Map as Map
import Data.List as List

data Player = Red | Black deriving (Eq)

instance Show Player where
    show Red = " R"
    show Black = " B"

data Move =  Jump Square Square | March Square Square

data Square = Square Int Int

instance Ord Square where
	(Square a b) <= (Square c d) = a < c || (a == c && b <= d)

instance Eq Square where
    (Square a b) == (Square c d) = a == c && b == d

data PieceType = Pawn | King

instance Show PieceType where
    show Pawn = " "
    show King = "K"

data Piece = Empty | Piece Player PieceType

instance Show Piece where
    show Empty = "   "
    show (Piece player piecetype) = show player ++ show piecetype

data BoardMap = BoardMap (Map Square Piece)

class Board board where
    seekBoard :: board -> Square -> Piece
    removePiece :: board -> Square -> board
    addPiece :: board -> Square -> Piece -> board



instance Board BoardMap where
    seekBoard = mSeekBoard
    removePiece = mRemovePiece
    addPiece = mAddPiece

instance Show BoardMap where
	show = mBShow

mBShow :: BoardMap -> String
mBShow (BoardMap squaresToPieces) = "   1  2  3  4  5  6  7  8\n" ++ showRows 1 (assocs squaresToPieces)

showRows :: Int -> [(Square, Piece)] -> String
showRows _ [] = ""
showRows rowNum rows = show rowNum ++ " " ++ (showRow $ (take 4 rows)) ++ (showRows (rowNum+1) (drop 4 rows))

showRow :: [(Square, Piece)] -> [Char]
showRow ((Square r c, piece):rest) = case mod r 2 of
	0 -> rowStr ++ "___\n"
	1 ->  "___" ++ rowStr ++ "\n"
	where
		rowStr = withEmptySpaces ((Square r c, piece):rest)

withEmptySpaces :: [(Square, Piece)] -> [Char]
withEmptySpaces row = concat $ ((intersperse "___" (List.map show pieces)))
	where
		pieces = List.map snd row

mSeekBoard :: BoardMap -> Square -> Piece
mSeekBoard (BoardMap b) sq = case Map.lookup sq b of
    Just piece -> piece
    Nothing -> Empty

mRemovePiece :: BoardMap -> Square -> BoardMap
mRemovePiece (BoardMap b) sq = BoardMap $ Map.delete sq b

mAddPiece :: BoardMap -> Square -> Piece -> BoardMap
mAddPiece (BoardMap b) sq piece = BoardMap $ Map.insert sq piece b

startGame :: BoardMap
startGame = addPieces (BoardMap (Map.empty)) squares
    where squares = [(Square x y) | x <- [1..8], y <- [1..8],
		(mod x 2 == 0 && mod y 2 == 1) || (mod x 2 == 1 && mod y 2 == 0)]

addPieces :: BoardMap -> [Square] -> BoardMap
addPieces (BoardMap m) [] = (BoardMap m)
addPieces (BoardMap m) (x:xs) = addPieces (addPiece (BoardMap m) x $ initialPieceAtSquare x) xs

initialPieceAtSquare :: Square -> Piece
initialPieceAtSquare (Square x y) | x <= 3 = (Piece Red Pawn)
                                  | x >= 6 = (Piece Black Pawn)
                                  | otherwise = Empty

main = do
    let s = startGame
    putStrLn "Hello"
    putStrLn( show s)
