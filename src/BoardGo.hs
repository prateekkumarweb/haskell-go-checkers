module BoardGo(
    createGame,
    Game(Game),
    playMove,
    validMove,
    getOppositeStone,
    Stone(Black, White),
    Point(Point),
    findTrappedGroup,
    removeKo,
    playPass,
    Move(Pass, Move),
    removeHopeless,
    getWinner,
    findAllTerritories,
    killGame,
    GameStatus(Alive, Dead, Over),
    seekBoard,
    removePiece,
    finishGame,
    getPiece
) where

import Data.Map as Map
import Data.List as List

data Point = Point Int Int deriving (Ord, Eq)

data Stone = Black | White | Ko | Empty  deriving (Eq, Show)

data Move = Pass Stone | Move Point Stone deriving (Eq)

getPiece :: Move -> Stone
getPiece (Pass st) = st
getPiece (Move  _ st) = st

data Game = Game {
    board :: Map Point Stone,
    lastMove :: Move,
    boardSize :: Int,
    scoreBlack :: Int,
    scoreWhite :: Int,
    status :: GameStatus
}

data GameStatus = Alive | Dead | Over deriving (Eq)

killGame :: Game -> Game
killGame game@(Game m lm s sb sw Alive) = Game m lm s (sb+1) sw Dead
killGame game = game

createGame :: Int -> Game
createGame size = Game{
    board = initalizeGameMap size,
    lastMove = Move (Point (-1) (-1)) White,
    boardSize = size,
    scoreBlack = 0,
    scoreWhite = 0,
    status = Alive
}

removeKo :: Game -> Game
removeKo game@(Game m lm s b w status) = (Game (removeKoFromMap (List.filter (\(p, s) -> s == Ko) (assocs m)) m) lm s b w status)

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
playPass game@(Game board lm s sb sw status) stone = Game {
    board = board,
    lastMove = Pass stone,
    boardSize = s,
    scoreBlack = newsb,
    scoreWhite = newsw,
    status = status
} where
    newsb = if stone == Black then sb else (sb+1)
    newsw = if stone == White then sw else (sw+1)

playMove :: Game -> Point -> Stone -> Game
playMove game@(Game board lm s sb sw status) point stone = removeGroups Game {
    board = addPiece board point stone,
    lastMove = Move point stone,
    boardSize = s,
    scoreBlack = sb,
    scoreWhite = sw,
    status = status
} point ostone where ostone = getOppositeStone stone

removeGroups :: Game -> Point -> Stone -> Game
removeGroups game@(Game board lm s sb sw status) point@(Point x y) stone | length pointsToBeRemoved == 1 && isKo = updateScore (addKo (removeStones game pointsToBeRemoved) (pointsToBeRemoved !! 0)) 1 (getOppositeStone stone)
                                          | otherwise = updateScore (removeStones game pointsToBeRemoved) (length pointsToBeRemoved) (getOppositeStone stone)
    where up = Point x (y+1)
          down = Point x (y-1)
          right = Point (x+1) y
          left = Point (x-1) y
          removeUp = removeDead up stone game
          removeDown = removeDead down stone game
          removeLeft = removeDead left stone game
          removeRight = removeDead right stone game
          pointsToBeRemoved = removeUp ++ removeDown ++ removeLeft ++ removeRight
          point' = if length removeUp == 1 then up else if length removeDown == 1 then down else if length removeLeft == 1 then left else right
          (up', down', left', right') = getAdj point'
          stone' = getOppositeStone stone
          isKo = length ((removeDead up' stone' game) ++ (removeDead down' stone' game) ++ (removeDead left' stone' game) ++ (removeDead right' stone' game)) == 1

getAdj :: Point -> (Point, Point, Point, Point)
getAdj (Point x y) = (Point x (y+1), Point x (y-1), Point (x-1) y, Point (x+1) y)

removeDead :: Point -> Stone -> Game -> [Maybe Point]
removeDead point stone game | checkIfTrapped game point stone = removablePoints
                            | otherwise = []
                            where removablePoints = (findTrappedGroup game point stone [])

updateScore :: Game -> Int -> Stone -> Game
updateScore game@(Game m lm s b w status) p st | st == Black = (Game m lm s (b+p) w status)
                                        | otherwise = (Game m lm s b (w+p) status)

addKo :: Game -> (Maybe Point) -> Game
addKo game@(Game m lm s b w status) (Just p) = (Game (addPiece m p Ko) lm s b w status)


removeStones :: Game -> [Maybe Point] -> Game
removeStones game@(Game m lm _ _ _ status) [] = game
removeStones game@(Game m lm s b w status) (Nothing:xs) = game
removeStones game@(Game m lm s b w status) ((Just p):xs) = removeStones (Game (removePiece m p) lm s b w status) xs

seekBoard :: Game -> Point -> Stone
seekBoard (Game m _ _ _ _ _) p = case Map.lookup p m of
    Just stone -> stone
    Nothing -> Empty

seekMap :: (Map Point Status) -> Point -> Status
seekMap m p = case Map.lookup p m of
    Just s -> s
    Nothing -> Unseen

findTrappedGroup :: Game -> Point -> Stone -> [Maybe Point] -> [Maybe Point]
findTrappedGroup game@(Game m move@(Move pt st) boardSize _ _ _) point@(Point x y) stone seenPoints
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

data Status = Seen | Unseen | SeenW | SeenB | None deriving (Eq)

findTerritory :: Game -> Point -> Stone -> ((Map Point Status), [Maybe Point]) -> ((Map Point Status), [Maybe Point])
findTerritory game@(Game _ _ boardSize _ _ _) point@(Point x y) stone (m, points)
    | x < 1 || x > boardSize || y < 1 || y > boardSize = (m, points)
    | elem (pure point) points = (m, points)
    | seekBoard game point == getOppositeStone stone = (m, Nothing:points)
    | seekBoard game point == stone = (m, points)
    | otherwise = findTerritory game left stone
        $ findTerritory game right stone
        $ findTerritory game up stone
        $ findTerritory game down stone ((Map.insert point Seen m), ((pure point):points))
    where up = Point x (y+1)
          down = Point x (y-1)
          right = Point (x+1) y
          left = Point (x-1) y

findTerritories :: Game -> Point -> (Map Point Status) -> (Map Point Status)
findTerritories game point m
    | seekBoard game point /= Empty && seekBoard game point /= Ko = addPiece m point None
    | seekMap m point == SeenW = m
    | seekMap m point == SeenB = m
    | seekMap m point == None = m
    | otherwise = if elem Nothing (snd tw) then
        if elem Nothing (snd tb) then setInMap m (snd tb) None
            else setInMap m (snd tb) SeenB
        else setInMap m (snd tw) SeenW
    where tb = findTerritory game point Black (m, [])
          tw = findTerritory game point White (m, [])

setInMap :: (Map Point Status) -> [Maybe Point] -> Status -> (Map Point Status)
setInMap m [] st = m
setInMap m (point:points) st | point /= Nothing = setInMap (addPiece m (purify point) st) points st
                             | otherwise = setInMap m points st

findAllTerritoriesOfPoints :: Game -> [Point] -> (Map Point Status) -> (Map Point Status)
findAllTerritoriesOfPoints game [] m = m
findAllTerritoriesOfPoints game (point:points) m = findAllTerritoriesOfPoints game points (findTerritories game point m)

findAllTerritories :: Game -> (Map Point Status)
findAllTerritories game@(Game _ _ boardSize _ _ _) = findAllTerritoriesOfPoints game points Map.empty
    where points = [(Point x y) | x <- [1..boardSize], y <- [1..boardSize]]

finishGame :: Game -> Game
finishGame game@(Game m lm s sb sw status) = Game m lm s sb' sw' Over
    where sb' = sb + countSeenB t s
          sw' = sw + countSeenW t s
          t = findAllTerritories game

countSeenB :: (Map Point Status) -> Int -> Int
countSeenB m size | l == size*size = 0
                  | otherwise = l
                  where l = length $ List.filter (\(p,s) -> s == SeenB) (assocs m)

countSeenW :: (Map Point Status) -> Int -> Int
countSeenW m size | l == size*size = 0
                  | otherwise = l
                  where l = length $ List.filter (\(p,s) -> s == SeenW) (assocs m)

purify :: Maybe a -> a
purify (Just a) = a

getOppositeStone :: Stone -> Stone
getOppositeStone stone | stone == Black = White
                       | stone == White = Black
                       | otherwise = Empty

validMove :: Game -> Point -> Stone -> Bool
validMove game@(Game m lm s _ _ _) p@(Point x y) st | x < 1 || x > s || y < 1 || y > s = False
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

getWinner :: Game -> String
getWinner game@(Game _ _ _ sb sw _) | sb > sw = "Black wins."
                                  | sw > sb = "White wins."
                                  | otherwise = "Game Draws"

removeHopeless :: Game -> [Point] -> Game
removeHopeless game@(Game m lm s sb sw status) [] = game
removeHopeless game@(Game m lm s sb sw status) (p:points) = removeHopeless Game{
  board = removePiece m p,
  lastMove = lm,
  boardSize = s,
  scoreBlack = newsb,
  scoreWhite = newsw,
  status = status
} points where
  newsb = if seekBoard game p == White then (sb+1) else sb
  newsw = if seekBoard game p == Black then (sw+1) else sw
