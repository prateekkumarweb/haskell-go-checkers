module BoardCheckers(
    Square(Square),
    Player(Red, Black),
    PieceType(Pawn, King),
    Piece(Empty, Piece),
    Move(March, Jump),
    isJump,
    getDestination,
    getSource,
    BoardMap(BoardMap),
    Board(seekBoard, validMoves, playMove),
    Game(Game),
    mValidJumps,
    startGame      
) where

import Data.Map as Map
import Data.List as List

-- | This data type represents a 'Player'
data Player = Red -- ^ To represent 'Red' 'Player'
            | Black -- ^ To represent 'Black' 'Player'
              deriving (Eq) -- Equable

-- | This data type represents a 'Move'
-- Can be either a 'Jump' or a 'March'
data Move = Jump Square Square -- ^ To represent 'Jump' from 'Square' to 'Square'
          | March Square Square -- ^ To represent 'March' from 'Square' to 'Square'
           deriving (Eq) -- Equable

-- | Take a 'Move' and decide if it is a 'Jump' or not
isJump :: Move -> Bool
isJump (Jump _ _ ) = True
isJump (March _ _) = False

-- | Take a 'Move' and return its destination 'Square'
getDestination :: Move -> Square
getDestination (March _ s) = s
getDestination (Jump _ s) = s

-- | Take a 'Move' and return its source 'Square'
getSource :: Move -> Square
getSource (March s _) = s
getSource (Jump s _) = s

-- | This data type represents a 'Square'
-- A 'Square' consists of two ints
data Square = Square Int Int

-- | Makes the 'Square' orderable
instance Ord Square where
    (Square a b) <= (Square c d) = a < c || (a == c && b <= d)

-- | Makes the 'Square' equable
instance Eq Square where
    (Square a b) == (Square c d) = a == c && b == d

-- | This data type represents a 'PieceType'
-- 'PieceType' can be either Pawn or King
data PieceType = Pawn -- ^ To represent Pawn
               | King -- ^ To represent King
                 deriving (Eq) -- Equable

-- | This data type represents 'Piece'
-- 'Piece' can either be empty or fully qualified by its 'PieceType' and 'Player'
data Piece = Empty -- ^ To represent Empty 'Piece', Empty pieces are not shown on board
           | Piece Player PieceType -- ^ To represent a 'Piece' denoted by its 'PieceType' and 'Player'
             deriving (Eq) -- Equable

-- | Test if the given 'Piece' is 'Red'
isRed :: Piece -> Bool
isRed Empty = False
isRed (Piece pl _) = pl == Red

-- | Test if the given 'Piece' is 'Black'
isBlack :: Piece -> Bool
isBlack Empty = False
isBlack (Piece pl _) = pl == Black

-- | This data type represents 'BoardMap'
data BoardMap = BoardMap (Map Square Piece) -- ^ 'BoardMap' is a Map from 'Square' to 'Piece'

-- | This data type represents 'Game'
data Game = Game BoardMap [Move] Player Square -- ^ A 'Game' consists of the 'BoardMap', possible moves of the current 'Player', the current 'Player' and the 'Square' which was clicked

-- | Class Board
class Board board where
    -- | Return the 'Piece' that is at the 'Square' in the board
    seekBoard :: board -> Square -> Piece
    -- | Remove the 'Piece' that is at the 'Square' in the board and return the new board
    removePiece :: board -> Square -> board
    -- | Add the 'Piece' at the 'Square' in the board and return the new board
    addPiece :: board -> Square -> Piece -> board
    -- | Replace the current 'Piece' by the new 'Piece' at the 'Square' in the board and return the new board
    replacePiece :: board -> Square -> Piece -> board
    -- | Return the valid moves of the specified 'Player'
    validMoves :: board -> Player -> [Move]
    -- | Play the 'Move' on the board and return the new board
    playMove :: board -> Move -> board

-- | Set corresponding member functions for the functions of the class
instance Board BoardMap where
    seekBoard = mSeekBoard
    removePiece = mRemovePiece
    addPiece = mAddPiece
    replacePiece = mReplacePiece
    validMoves = mValidMoves
    playMove = mPlayMove

-- Implementation of the Class's functions ------

-- | Return the 'Piece' that is at the 'Square' in the board
mSeekBoard :: BoardMap -> Square -> Piece
mSeekBoard (BoardMap b) sq = case Map.lookup sq b of
    Just piece -> piece
    Nothing -> Empty

-- | Remove the 'Piece' that is at the 'Square' in the board and return the new 'BoardMap'
mRemovePiece :: BoardMap -> Square -> BoardMap
mRemovePiece (BoardMap b) sq = BoardMap $ Map.delete sq b

-- | Add the 'Piece' at the 'Square' in the board and return the new 'BoardMap'
mAddPiece :: BoardMap -> Square -> Piece -> BoardMap
mAddPiece (BoardMap b) sq piece = BoardMap $ Map.insert sq piece b

-- | Replace the current 'Piece' by the new 'Piece' at the 'Square' in the 'BoardMap' and return the new 'BoardMap'
mReplacePiece :: BoardMap -> Square -> Piece -> BoardMap
mReplacePiece b sq piece = addPiece ( removePiece b sq) sq piece


--- Functions that start the game --------

-- | Return the initial 'BoardMap'
startGame :: BoardMap
-- Add appropriate 'Piece' at all 'Square' of the Board
startGame = addPieces (BoardMap (Map.empty)) squares
    where squares = [(Square x y) | x <- [1..8], y <- [1..8], (mod x 2 == 0 && mod y 2 == 1) || (mod x 2 == 1 && mod y 2 == 0)]

-- | Takes an empty 'BoardMap' and a list of 'Square', adds appropeiate 'Piece' at the squares and returns the new 'BoardMap'
addPieces :: BoardMap -> [Square] -> BoardMap
addPieces (BoardMap m) [] = (BoardMap m)
addPieces (BoardMap m) (x:xs) = addPieces (addPiece (BoardMap m) x $ initialPieceAtSquare x) xs

-- | Takes a 'Square' and returns the 'Piece' that the board has at that 'Square' in the beginning of game
initialPieceAtSquare :: Square -> Piece
initialPieceAtSquare (Square x y) | y <= 3 = (Piece Red Pawn)
                                  | y >= 6 = (Piece Black Pawn)
                                  | otherwise = Empty

--- Member Functions for actual moves ------------

-- | Play the 'Move' on the board and return the new board
mPlayMove :: BoardMap -> Move -> BoardMap
-- During Jump remove the current piece at the current positon, add it at the final position and remove the piece jumped over. Check for promotion to King
mPlayMove board (Jump s1@(Square x1 y1) s2@(Square x2 y2)) | y2 /= 8 && y2 /= 1 = replacePiece (replacePiece (replacePiece board s2 (seekBoard board s1)) s1 Empty) (Square (quot (x1+x2) 2) (quot (y1+y2) 2)) Empty
                                                         | otherwise = replacePiece (replacePiece (replacePiece board s2 (promote $ seekBoard board s1)) s1 Empty) (Square (quot (x1+x2) 2) (quot (y1+y2) 2)) Empty
-- During Move remove the current piece at the current positon, add it at the final position. Check for promotion to King
mPlayMove board (March s1@(Square x1 y1) s2@(Square x2 y2)) | y2 /= 8 && y2 /= 1 = replacePiece (replacePiece board s2 (seekBoard board s1)) s1 Empty
                                                          | otherwise = replacePiece (replacePiece board s2 (promote (seekBoard board s1))) s1 Empty

-- | Promote the Piece pass as argument to a King Piece
promote :: Piece -> Piece
promote (Piece pl ty) = Piece pl King
promote Empty = Empty

-- | Return the valid moves for the 'Player' passed as argument
mValidMoves :: BoardMap -> Player -> [Move]
mValidMoves b pl | length jumpsList > 0 = jumpsList -- If jump is possible then only jumps are allowed
               | otherwise = marchList -- Else return the march list
               where jumpsList = List.foldr (++) [] (List.map (mValidJumps b) (mPlayableSquares b pl)) -- Get the valid jumps for all the Pieces of the current 'Player'
                     marchList = List.foldr (++) [] (List.map (mValidMarches b) (mPlayableSquares b pl)) -- Get the valid jumps for all the Pieces of the current 'Player'

-- | Return a list of all the 'Square' that have the Piece of the currrent 'Player'
mPlayableSquares :: BoardMap -> Player -> [(Square, Piece)]
mPlayableSquares (BoardMap b) pl = List.filter (isSamePlayer pl) (toList b)

-- | Is the 'Player' same the 'Player' of the Piece ?
isSamePlayer :: Player -> (Square, Piece) -> Bool
isSamePlayer p (_, Piece p2 _) = p == p2
isSamePlayer p (_,Empty) = False

-- Functions to calculate the possible jumps -----

-- | Return the valid jumps from the passed 'Square' of the passed 'Player'
mValidJumps :: BoardMap -> (Square, Piece) -> [Move]
mValidJumps b (s@(Square x y), piece@(Piece player ptype)) | player == Black && ptype == Pawn && y >= 3 = validJumpsBlackPawn b s -- Get valid jumps for Black Pawn
                                                         | player == Black && ptype == King = validJumpsBlackKing b s -- Get valid jumps for Black King
                                                         | player == Red && ptype == Pawn && y <= 6 = validJumpsRedPawn b s -- Get valid jumps for Red Pawn
                                                         | player == Red && ptype == King = validJumpsRedKing b s -- Get valid jumps for Red King
                                                         | otherwise = []
mValidJumps _ (_, Empty) = []

-- | Get valid jumps for the passed Black Pawn
validJumpsBlackPawn :: BoardMap -> Square -> [Move]
-- Check if it is possible to Jump in the two directions, if so add to the List
validJumpsBlackPawn board s@(Square x y) = [Jump (Square x y) (Square a b) | (a, b) <- [(x-2, y-2), (x+2, y-2)], a >= 1 && a <= 8 && b >= 1 && b <= 8, seekBoard board (Square a b) == Empty, isRed $ seekBoard board (Square (quot (a+x) 2) (quot (b+y) 2))]

-- | Get valid jumps for the passed Red Pawn
validJumpsRedPawn :: BoardMap -> Square -> [Move]
-- Check if it is possible to Jump in the two directions, if so add to the List
validJumpsRedPawn board s@(Square x y) = [Jump (Square x y) (Square a b) | (a, b) <- [(x-2, y+2), (x+2, y+2)], a >= 1 && a <= 8 && b >= 1 && b <= 8, seekBoard board (Square a b) == Empty, isBlack $ seekBoard board (Square (quot (a+x) 2) (quot (b+y) 2))]

-- | Get valid jumps for the passed Black King
validJumpsBlackKing :: BoardMap -> Square -> [Move]
-- Check if it is possible to Jump in the four directions, if so add to the List
validJumpsBlackKing board s@(Square x y) = [Jump (Square x y) (Square a b) | a <- [x-2, x+2], b <- [y-2, y+2], a >= 1 && a <= 8 && b >= 1 && b <= 8, seekBoard board (Square a b) == Empty, isRed $ seekBoard board (Square (quot (a+x) 2) (quot (b+y) 2))]

-- | Get valid jumps for the passed Red King
validJumpsRedKing :: BoardMap -> Square -> [Move]
-- Check if it is possible to Jump in the four directions, if so add to the List
validJumpsRedKing board s@(Square x y) = [Jump (Square x y) (Square a b) | a <- [x-2, x+2], b <- [y-2, y+2], a >= 1 && a <= 8 && b >= 1 && b <= 8, seekBoard board (Square a b) == Empty, isBlack $ seekBoard board (Square (quot (a+x) 2) (quot (b+y) 2))]

-- Functions to calculate the possible marches -----

-- | Return the valid marches from the passed Square of the passed 'Player'
mValidMarches :: BoardMap -> (Square, Piece) -> [Move]
mValidMarches b (s@(Square x y), piece@(Piece player ptype)) | player == Black && ptype == Pawn && y >= 2 = validMarchesBlackPawn b s -- Get valid marches for Black Pawn
                                                           | ptype == King = validMarchesKing b s -- Get valid marches for King
                                                           | player == Red && ptype == Pawn && y <= 7 = validMarchesRedPawn b s -- Get valid marches for Red Pawn
                                                           | otherwise = []

-- | Get valid marches for the passed King
validMarchesKing :: BoardMap -> Square -> [Move]
-- Check if it is possible to March in the four directions, if so add to the List
validMarchesKing board s@(Square x y) = [March (Square x y) (Square a b) | a <- [x-1, x+1], b <- [y-1, y+1], a >= 1 && a <= 8 && b >= 1 && b <= 8, seekBoard board (Square a b) == Empty]

-- | Get valid marches for the passed Black Pawn
validMarchesBlackPawn :: BoardMap -> Square -> [Move]
-- Check if it is possible to March in the two directions, if so add to the List
validMarchesBlackPawn board s@(Square x y) = [March (Square x y) (Square a b) | (a, b) <- [(x-1, y-1), (x+1, y-1)], a >= 1 && a <= 8 && b >= 1 && b <= 8, seekBoard board (Square a b) == Empty]

-- | Get valid marches for the passed Red Pawn
validMarchesRedPawn :: BoardMap -> Square -> [Move]
-- Check if it is possible to March in the two directions, if so add to the List
validMarchesRedPawn board s@(Square x y) = [March (Square x y) (Square a b) | (a, b) <- [(x-1, y+1), (x+1, y+1)], a >= 1 && a <= 8 && b >= 1 && b <= 8, seekBoard board (Square a b) == Empty]
