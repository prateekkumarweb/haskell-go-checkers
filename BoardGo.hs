import Data.Map as Map
import Data.List as List

data Point = Point Int Int deriving (Ord, Eq, Show)

data Stone = Black | White | Ko | Empty  deriving (Eq)

data Game = Game {
    board :: Map Point Stone,
    lastMove :: (Point, Stone),
    size :: Int
}

createGame :: Game
createGame = Game{
    board = initalizeGameMap ,
    lastMove = ( Point -1 -1 , Red),
    size = 19
}

addPiece :: Map -> Point -> Stone -> Map
addPiece m point stone = Map.insert point stone (Map.delete point m)

removePiece :: Map -> Point -> Map
removePiece m point = addPiece m point Empty

initalizeGameMap :: Int -> Map
initalizeGameMap size = addPieces (Map.empty) points
    where points = [(Point x y) | x <- [1..size], y <- [1..size]]

addPieces :: Map -> [Point] -> Map
addPieces m [] = m
addPieces m (x:xs) = addPieces (Map.insert x Empty m) xs

playMove :: Game -> Point -> Stone -> Game
playMove game@(Game board lm s) point stone = Game {
    board = addPiece board point stone,
    lastMove = (point, stone),
    size = s
}

seekBoard :: Game -> Point -> Stone
seekBoard (Game m _ _) p = case Map.lookup p main of
    Just stone -> stone
    Nothing -> Empty

findTrappedGroup :: Game -> Point -> [Point] -> [Point]
findTrappedGroup game@(Game m (pt, st) size) point@(Point x y) seenPoints =
    | x < 1 || x > size || y < 1 || y > size  = seenPoints
    | elem point seenPoints = seenPoints
    | seekBoard game point == Empty = Empty:seenPoints
    | seekBoard game point == Ko = Empty:seenPoints
    | seekBoard game point == st = seenPoints
    | otherwise = findTrappedGroup game left
    .findTrappedGroup game right
    .findTrappedGroup game up
    .findTrappedGroup game down (point:seenPoints)
    where up = Point x y+1
          down = Point x y-1
          right = Point x+1 y
          left = Point x-1 y


































.
