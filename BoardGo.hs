module BoardGo(
    createGame,
    Game(Game),
    playMove,
    validMove,
    getOppositeStone,
    Stone(Black, White),
    Point(Point),
    findTrappedGroup,
    getBlackScore,
    getWhiteScore,
    removeKo,
    playPass,
    getLastMove,
    Move(Pass, Move),
    removeHopeless,
    getWinner
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

data Move = Pass | Move Point Stone deriving (Eq)

data Game = Game {
    board :: Map Point Stone,
    lastMove :: Move,
    boardSize :: Int,
    scoreBlack :: Int,
    scoreWhite :: Int
}

getLastMove :: Game -> Move
getLastMove (Game _ lm _ _ _) = lm

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

removeKo :: Game -> Game
removeKo game@(Game m lm s b w) = (Game (removeKoFromMap (List.filter (\(p, s) -> s == Ko) (assocs m)) m) lm s b w)

removeKoFromMap :: [(Point, Stone)] -> (Map Point Stone) -> (Map Point Stone)
removeKoFromMap [] m = m
removeKoFromMap ((p,s):xs) m = addPiece m p Empty

addPiece :: (Map Point t) -> Point -> t -> (Map Point t)
addPiece m point stone = Map.insert point stone (Map.delete point m)

removePiece :: (Map Point Stone) -> Point -> (Map Point Stone)
removePiece m point = addPiece m point Empty

initalizeGameMap :: Int -> (Map Point Stone)
initalizeGameMap boardSize = addPieces (Map.empty) points
    where points = [(Point x y) | x <- [1..boardSize], y <- [1..boardSize]]

addPieces :: (Map Point Stone) -> [Point] -> (Map Point Stone)
addPieces m [] = m
addPieces m (x:xs) = addPieces (Map.insert x Empty m) xs

playPass :: Game -> Stone -> Game
playPass game@(Game board lm s sb sw) stone = Game {
    board = board,
    lastMove = Pass,
    boardSize = s,
    scoreBlack = newsb,
    scoreWhite = newsw
} where
    newsb = if stone == Black then sb else (sb+1)
    newsw = if stone == White then sw else (sw+1)

playMove :: Game -> Point -> Stone -> Game
playMove game@(Game board lm s sb sw) point stone = removeGroups Game {
    board = addPiece board point stone,
    lastMove = Move point stone,
    boardSize = s,
    scoreBlack = sb,
    scoreWhite = sw
} point ostone where ostone = getOppositeStone stone

removeGroups :: Game -> Point -> Stone -> Game
removeGroups game point@(Point x y) stone | length pointsToBeRemoved == 1 = updateScore (addKo (removeStones game pointsToBeRemoved) (pointsToBeRemoved !! 0)) 1 (getOppositeStone stone)
                                          | otherwise = updateScore (removeStones game pointsToBeRemoved) (length pointsToBeRemoved) (getOppositeStone stone)
    where up = Point x (y+1)
          down = Point x (y-1)
          right = Point (x+1) y
          left = Point (x-1) y
          pointsToBeRemoved = (removeDead up stone game) ++ (removeDead down stone game) ++ (removeDead left stone game) ++ (removeDead right stone game)

removeDead :: Point -> Stone -> Game -> [Maybe Point]
removeDead point stone game | checkIfTrapped game point stone = removablePoints
                            | otherwise = []
                            where removablePoints = (findTrappedGroup game point stone [])

updateScore :: Game -> Int -> Stone -> Game
updateScore game@(Game m lm s b w) p st | st == Black = (Game m lm s (b+p) w)
                                        | otherwise = (Game m lm s b (w+p))

addKo :: Game -> (Maybe Point) -> Game
addKo game@(Game m lm s b w) (Just p) = (Game (addPiece m p Ko) lm s b w)


removeStones :: Game -> [Maybe Point] -> Game
removeStones game@(Game m lm _ _ _) [] = game
removeStones game@(Game m lm s b w) (Nothing:xs) = game
removeStones game@(Game m lm s b w) ((Just p):xs) = removeStones (Game (removePiece m p) lm s b w) xs

seekBoard :: Game -> Point -> Stone
seekBoard (Game m _ _ _ _) p = case Map.lookup p m of
    Just stone -> stone
    Nothing -> Empty

seekMap :: (Map Point Status) -> Point -> Status
seekMap m p = case Map.lookup p m of
    Just s -> s
    Nothing -> Unseen

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

data Status = Seen | Unseen | SeenW | SeenB | None

-- findTerritory :: Game -> Point -> Stone -> ((Map Point Status), [Maybe Point]) -> ((Map Point Status), [Maybe Point])
-- findTerritory game@(Game _ _ boardSize _ _) point@(Point x y) stone (m, points)
--     | x < 1 || x > boardSize || y < 1 || y > boardSize = (m, points)
--     | elem (pure point) points = (m, points)
--     | seekBoard game point == getOppositeStone stone = (m, Nothing:points)
--     | seekBoard game point == stone = (m, points)
--     | otherwise = findTerritory game left stone
--         $ findTerritory game right stone
--         $ findTerritory game up stone
--         $ findTerritory game down stone ((Map.insert point Seen m), ((pure point):points))
--     where up = Point x (y+1)
--           down = Point x (y-1)
--           right = Point (x+1) y
--           left = Point (x-1) y
--
-- findTerritories :: Game -> Point -> (Map Point Status) -> (Map Point Status)
-- findTerritories game point m
--     | seekBoard game point /= Empty = addPiece m point None
--     | seekMap m point == SeenW = m
--     | seekMap m point == SeenB = m
--     | seekMap m point == None = m
--     | if elem Nothing (snd tw) then
--         if elem Nothing (snd tb) then setInMap (fst tb) None
--             else setInMap (fst tb) SeenB
--         else setInMap (fst tw) SeenW
--     where tb = findTerritory game point Black (m, [])
--         tw = findTerritory game point White (m, [])
--
-- setInMap :: ((Map Point Status), [Maybe Point]) -> Status -> (Map Point Status)

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

getWinner :: Game -> String
getWinner game@(Game _ _ _ sb sw) | sb > sw = "Black wins."
                                  | sw > sb = "White wins."
                                  | otherwise = "Game Draws"

removeHopeless :: Game -> [Point] -> Game
removeHopeless game@(Game m lm s sb sw) [] = game
removeHopeless game@(Game m lm s sb sw) (p:points) = removeHopeless Game{
  board = removePiece m p,
  lastMove = lm,
  boardSize = s,
  scoreBlack = newsb,
  scoreWhite = newsw
} points where
  newsb = if seekBoard game p == White then (sb+1) else sb
  newsw = if seekBoard game p == Black then (sw+1) else sw
