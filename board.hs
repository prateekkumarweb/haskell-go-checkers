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

data PieceType = Pawn | King deriving (Eq)

instance Show PieceType where
    show Pawn = " "
    show King = "K"

data Piece = Empty | Piece Player PieceType deriving (Eq)

isRed :: Piece -> Bool
isRed Empty = False
isRed (Piece pl _) = pl == Red

isBlack :: Piece -> Bool
isBlack Empty = False
isBlack (Piece pl _) = pl == Black

instance Show Piece where
    show Empty = "   "
    show (Piece player piecetype) = show player ++ show piecetype

data BoardMap = BoardMap (Map Square Piece)

class Board board where
    seekBoard :: board -> Square -> Piece
    removePiece :: board -> Square -> board
    addPiece :: board -> Square -> Piece -> board
    replacePiece :: board -> Square -> Piece -> board
    validMoves :: board -> Player -> [Move]

instance Board BoardMap where
    seekBoard = mSeekBoard
    removePiece = mRemovePiece
    addPiece = mAddPiece
    replacePiece = mReplacePiece
    validMoves = mValidMoves

--member fns --------------------------------------------------------------
mValidMoves :: BoardMap -> Player -> [Move]
mValidMoves (BoardMap b) pl = []

mPlayableSquares :: BoardMap -> Player -> [(Square, Piece)]
mPlayableSquares (BoardMap b) pl = List.filter (isSamePlayer pl) (toList b)

isSamePlayer :: Player -> (Square, Piece) -> Bool
isSamePlayer p (_, Piece p2 _) = p == p2

mValidJumps :: BoardMap -> (Square, Piece) -> [Move]
mValidJumps b (s@(Square x y), piece@(Piece player ptype)) | player == Black && ptype == Pawn && y >= 3 = validJumpsBlackPawn b s
                                                                      | player == Black && ptype == King = validJumpsBlackKing b s
                                                                      | player == Red && ptype == Pawn && y <= 6 = validJumpsRedPawn b s
                                                                      | player == Red && ptype == King = validJumpsRedKing b s
                                                                      | otherwise = []

validJumpsBlackPawn :: BoardMap -> Square -> [Move]
validJumpsBlackPawn board s@(Square x y) = [Jump (Square x y) (Square a b) | (a, b) <- [(x-2, y-2), (x+2, y-2)], seekBoard board (Square a b) == Empty, isRed $ seekBoard board (Square (quot (a+x) 2) (quot (b+y) 2))]

validJumpsRedPawn :: BoardMap -> Square -> [Move]
validJumpsRedPawn board s@(Square x y) = [Jump (Square x y) (Square a b) | (a, b) <- [(x-2, y+2), (x+2, y+2)], seekBoard board (Square a b) == Empty, isBlack $ seekBoard board (Square (quot (a+x) 2) (quot (b+y) 2))]

validJumpsBlackKing :: BoardMap -> Square -> [Move]
validJumpsBlackKing board s@(Square x y) = [Jump (Square x y) (Square a b) | a <- [x-2, x+2], b <- [y-2, y+2], a >= 1 && a <= 8 && b >= 1 && b <= 8, seekBoard board (Square a b) == Empty, isRed $ seekBoard board (Square (quot (a+x) 2) (quot (b+y) 2))]

validJumpsRedKing :: BoardMap -> Square -> [Move]
validJumpsRedKing board s@(Square x y) = [Jump (Square x y) (Square a b) | a <- [x-2, x+2], b <- [y-2, y+2], a >= 1 && a <= 8 && b >= 1 && b <= 8, seekBoard board (Square a b) == Empty, isBlack $ seekBoard board (Square (quot (a+x) 2) (quot (b+y) 2))]

mSeekBoard :: BoardMap -> Square -> Piece
mSeekBoard (BoardMap b) sq = case Map.lookup sq b of
    Just piece -> piece
    Nothing -> Empty

mRemovePiece :: BoardMap -> Square -> BoardMap
mRemovePiece (BoardMap b) sq = BoardMap $ Map.delete sq b

mAddPiece :: BoardMap -> Square -> Piece -> BoardMap
mAddPiece (BoardMap b) sq piece = BoardMap $ Map.insert sq piece b

mReplacePiece :: BoardMap -> Square -> Piece -> BoardMap
mReplacePiece b sq piece = addPiece ( removePiece b sq) sq piece

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

-- Show for Board -----------------------------------------------------
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



main = do
    let s = startGame
    putStrLn "Hello"
    putStrLn( show s)
