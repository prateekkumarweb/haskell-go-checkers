module BoardGo(
    createGame,
    Game(board),
    playMove,
    validMove,
    getOppositeStone,
    Stone(Black, White),
    Point(Point),
    findTrappedGroup,
    getBlackScore,
    getWhiteScore
) where

import Data.Map as Map
import Data.List as List

data Point = Point Int Int deriving (Ord, Eq, Show)

data Stone = Black | White | Ko | Empty  deriving (Eq)

instance Show Stone where
    show Black = " b "
    show White = " w "
    show Ko = " k "
    show Empty = " . "

data Move = Pass | Move Point Stone

data Game = Game {
    board :: Map Point Stone,
    lastMove :: Move,
    boardSize :: Int,
    scoreBlack :: Int,
    scoreWhite :: Int
}

getBlackScore :: Game -> Int
getBlackScore (Game _ _ _ b _) = b

getWhiteScore :: Game -> Int
getWhiteScore (Game _ _ _ _ w) = w

createGame :: Game
createGame = Game{
    board = initalizeGameMap 19,
    lastMove = Move (Point (-1) (-1)) Black,
    boardSize = 19,
    scoreBlack = 0,
    scoreWhite = 0
}

addPiece :: (Map Point Stone) -> Point -> Stone -> (Map Point Stone)
addPiece m point stone = Map.insert point stone (Map.delete point m)

removePiece :: (Map Point Stone) -> Point -> (Map Point Stone)
removePiece m point = addPiece m point Empty

initalizeGameMap :: Int -> (Map Point Stone)
initalizeGameMap boardSize = addPieces (Map.empty) points
    where points = [(Point x y) | x <- [1..boardSize], y <- [1..boardSize]]

addPieces :: (Map Point Stone) -> [Point] -> (Map Point Stone)
addPieces m [] = m
addPieces m (x:xs) = addPieces (Map.insert x Empty m) xs

playMove :: Game -> Point -> Stone -> Game
playMove game@(Game board lm s sb sw) point stone = removeGroups Game {
    board = addPiece board point stone,
    lastMove = Move point stone,
    boardSize = s,
    scoreBlack = sb,
    scoreWhite = sw
} point ostone where ostone = getOppositeStone stone

removeGroups :: Game -> Point -> Stone -> Game
removeGroups game point@(Point x y) stone = removeDead up stone $ removeDead down stone $ removeDead left stone $ removeDead right stone game
    where up = Point x (y+1)
          down = Point x (y-1)
          right = Point (x+1) y
          left = Point (x-1) y

removeDead :: Point -> Stone -> Game -> Game
removeDead point stone game | checkIfTrapped game point stone = updateScore (removeStones game removablePoints) (length removablePoints) (getOppositeStone stone)
                            | otherwise = game
                            where removablePoints = (findTrappedGroup game point stone [])

updateScore :: Game -> Int -> Stone -> Game
updateScore game@(Game m lm s b w) p st | st == Black = (Game m lm s (b+p) w)
                                        | otherwise = (Game m lm s b (w+p))


removeStones :: Game -> [Maybe Point] -> Game
removeStones game@(Game m lm _ _ _) [] = game
removeStones game@(Game m lm s b w) (Nothing:xs) = game
removeStones game@(Game m lm s b w) ((Just p):xs) = removeStones (Game (removePiece m p) lm s b w) xs

seekBoard :: Game -> Point -> Stone
seekBoard (Game m _ _ _ _) p = case Map.lookup p m of
    Just stone -> stone
    Nothing -> Empty

findTrappedGroup :: Game -> Point -> Stone -> [Maybe Point] -> [Maybe Point]
findTrappedGroup game@(Game m move@(Move pt st) boardSize _ _) point@(Point x y) stone seenPoints
    | x < 1 || x > boardSize || y < 1 || y > boardSize  = seenPoints
    | elem (pure point) seenPoints = seenPoints
    | seekBoard game point == Empty = Nothing:seenPoints
    | seekBoard game point == Ko = Nothing:seenPoints
    | seekBoard game point /= stone = seenPoints
    | otherwise = findTrappedGroup game left stone
        $ findTrappedGroup game right stone
        $ findTrappedGroup game up stone
        $ findTrappedGroup game down stone ((pure point):seenPoints)
    where up = Point x (y+1)
          down = Point x (y-1)
          right = Point (x+1) y
          left = Point (x-1) y

getOppositeStone :: Stone -> Stone
getOppositeStone stone | stone == Black = White
                       | stone == White = Black
                       | otherwise = Empty

validMove :: Game -> Point -> Stone -> Bool
validMove game@(Game m lm s _ _) p@(Point x y) st | x < 1 || x > s || y < 1 || y > 19 = False
    | seekBoard game p /= Empty = False
    | not $ checkIfTrapped game1 p st = True
    | (seekBoard game up == ostone) && (checkIfTrapped game1 up ostone) = True
    | (seekBoard game down == ostone) && (checkIfTrapped game1 down ostone) = True
    | (seekBoard game left == ostone) && (checkIfTrapped game1 left ostone) = True
    | (seekBoard game right == ostone) && (checkIfTrapped game1 right ostone) = True
    | otherwise = False
    where game1 = playMove game p st
          ostone = getOppositeStone st
          up = Point x (y+1)
          down = Point x (y-1)
          right = Point (x+1) y
          left = Point (x-1) y

checkIfTrapped :: Game -> Point -> Stone -> Bool
checkIfTrapped game p st = not $ elem Nothing (findTrappedGroup game p st [])

-- checkIfTrapped :: Game -> Point -> Stone -> Bool
-- checkIfTrapped game p st | length (List.filter checkIfNothing (findTrappedGroup game p st [])) == 0 = True
--                          | otherwise = False

checkIfNothing :: (Maybe Point) -> Bool
checkIfNothing Nothing = True
checkIfNothing (Just point) = False

instance Show Game where
  show = mBShow

mBShow :: Game -> String
mBShow game@(Game m _ s _ _) = "   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19\n" ++ showRows 1 (assocs m)

showRows :: Int -> [(Point, Stone)] -> String
showRows _ [] = ""
showRows rowNum rows = show rowNum ++ " " ++ (showRow $ (take 19 rows)) ++ (showRows (rowNum+1) (drop 19 rows))

showRow :: [(Point, Stone)] -> [Char]
showRow ((Point r c, piece):rest) = rowStr ++ "\n"
  where
      rowStr = withEmptySpaces ((Point r c, piece):rest)

withEmptySpaces :: [(Point, Stone)] -> [Char]
withEmptySpaces row = concat $ ((intersperse "" (List.map show pieces)))
  where
      pieces = List.map snd row
