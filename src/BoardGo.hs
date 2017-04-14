{-|
    This module exports all the functions required by 'playGo'
    function in 'Go' game.
-}
module BoardGo(
    Point(Point),
    Stone(Black, White, Ko, Empty),
    getOppositeStone,
    Move(Pass, Move),
    getPiece,
    GameStatus(Alive, Dead, Over),
    Game(Game),
    createGame,
    killGame,
    seekBoard,
    removeKo,
    removePiece,
    playPass,
    validMove,
    playMove,
    finishGame,
    getWinner
) where

import Data.Map as Map
import Data.List as List

-- | This data type represents a 'Point' in Board of Go (i.e. intersection of two lines)
data Point = Point Int Int -- ^ Point constructor takes two coordinates x and y
           deriving (Ord, Eq)

-- | This data type represents 'Stone' placed on the board
data Stone = Black -- ^ To represent 'Black' stone
           | White -- ^ To represent 'White' stone
           | Ko -- ^ 'Ko' is not shown on the board. Placed when Ko situation occurs
           | Empty -- ^ To represent that nothing is placed on the point
           deriving (Eq, Show)

-- | 'getOppositeStone' returns opposite stone of given 'Stone'
getOppositeStone :: Stone -> Stone
getOppositeStone stone | stone == Black = White
                      | stone == White = Black
                      | otherwise = Empty

-- | This data type represents a 'Move' that can be performed on the board by a player
data Move = Pass Stone -- ^ Player with this 'Stone' plays pass
          | Move Point Stone -- ^ Player with this 'Stone' plays on a 'Point'
          deriving (Eq)

-- | 'getPiece' returns the 'Stone' from a given 'Move'
getPiece :: Move -> Stone
getPiece (Pass st) = st
getPiece (Move  _ st) = st

-- | This data type reprsents 'Game'
data Game = Game {
    board :: Map Point Stone, -- ^ Map from point to stone
    lastMove :: Move, -- ^ Last move that was played in the game
    boardSize :: Int, -- ^ Board size 9 or 13 or 19
    scoreBlack :: Int, -- ^ Score of Black stone
    scoreWhite :: Int, -- ^ Score of White stone
    status :: GameStatus -- ^ Status of the game whether game is running or over
}

-- | 'GameStatus' represents state of the game if game is alive or dead or over
data GameStatus = Alive -- ^ game is running
                | Dead -- ^ game is over and hopeless string is counted
                | Over -- ^ Game over and results are shown on the screen
                deriving (Eq)

-- | 'createGame' takes a size and returns a 'Game' object
createGame :: Int -> Game
createGame size = Game{
    board = addPieces (Map.empty) points,
    lastMove = Move (Point (-1) (-1)) White,
    boardSize = size,
    scoreBlack = 0,
    scoreWhite = 0,
    status = Alive
} where points = [(Point x y) | x <- [1..size], y <- [1..size]]

-- | 'addPieces' adds Empty to set of points in a map
addPieces :: (Map Point Stone) -> [Point] -> (Map Point Stone)
addPieces m [] = m
addPieces m (x:xs) = addPieces (Map.insert x Empty m) xs

-- | 'killGame' changes game status from 'Alive' to 'Dead' and increases the score of 'Black'
-- as 'White' plays the last 'Pass'.
killGame :: Game -> Game
killGame game@(Game m lm s sb sw Alive) = Game m lm s (sb+1) sw Dead
killGame game = game

-- | 'removeKo' removes 'Ko' from the 'Game'
removeKo :: Game -> Game
removeKo game@(Game m lm s b w status) = (Game newBoard lm s b w status)
    where pointsWithKo = List.filter (\(p, s) -> s == Ko) (assocs m)
          newBoard = if pointsWithKo == [] then m else addPiece m (fst (pointsWithKo !! 0)) Empty

-- | 'addPiece' adds a element of type t to map
addPiece :: (Map Point t) -> Point -> t -> (Map Point t)
addPiece m point stone = Map.insert point stone (Map.delete point m)

-- | 'removePiece' removes the stone from given point in the map
removePiece :: (Map Point Stone) -> Point -> (Map Point Stone)
removePiece m point = addPiece m point Empty

-- | 'playPass' adds 'Pass' move by the 'Stone' on the 'Game'
playPass :: Game -> Stone -> Game
playPass game@(Game board lm s sb sw status) stone = Game {
    board = board,
    lastMove = Pass stone,
    boardSize = s,
    scoreBlack = newsb,
    scoreWhite = newsw,
    status = status
} where
    -- | Increase score of opposite stone
    newsb = if stone == Black then sb else (sb+1)
    newsw = if stone == White then sw else (sw+1)

-- | 'playMove' adds 'Move' to 'Point' by 'Stone' on the 'Game'.
-- After playing the move, also removes the captured points of the opposite stone if present
playMove :: Game -> Point -> Stone -> Game
playMove game@(Game board lm s sb sw status) point stone = removeGroups Game {
    board = addPiece board point stone,
    lastMove = Move point stone,
    boardSize = s,
    scoreBlack = sb,
    scoreWhite = sw,
    status = status
} point ostone where ostone = getOppositeStone stone

-- | 'removeGroups' removes captured stones of 'Stone' from the 'Game' starting from
-- 'Point' and moving in all four directions
removeGroups :: Game -> Point -> Stone -> Game
removeGroups game@(Game board lm s sb sw status) point@(Point x y) stone
    -- If number of points captured is one then check if Ko is formed and add Ko to the board if neccesary
    | length pointsToBeRemoved == 1 && isKo = updateScore (addKo (removeStones game pointsToBeRemoved) (pointsToBeRemoved !! 0)) 1 stone'
    | otherwise = updateScore (removeStones game pointsToBeRemoved) (length pointsToBeRemoved) stone'
    where (up, down, left, right) = getAdj point
          removeUp = removeDead up stone game
          removeDown = removeDead down stone game
          removeLeft = removeDead left stone game
          removeRight = removeDead right stone game
          pointsToBeRemoved = removeUp ++ removeDown ++ removeLeft ++ removeRight
          point' = if length removeUp == 1 then up else if length removeDown == 1 then down else if length removeLeft == 1 then left else right
          (up', down', left', right') = getAdj point'
          stone' = getOppositeStone stone
          isKo = length ((removeDead up' stone' game) ++ (removeDead down' stone' game) ++ (removeDead left' stone' game) ++ (removeDead right' stone' game)) == 1

-- | 'getAdj' returns adjacent points of a given 'Point'
getAdj :: Point -> (Point, Point, Point, Point)
getAdj (Point x y) = (Point x (y+1), Point x (y-1), Point (x-1) y, Point (x+1) y)

-- | 'removeDead' returns dead groups of 'Stone' in 'Game' starting from 'Point'
removeDead :: Point -> Stone -> Game -> [Maybe Point]
removeDead point stone game | checkIfTrapped game point stone = removablePoints
                            | otherwise = []
                            where removablePoints = (findTrappedGroup game point stone [])

-- | 'updateScore' updates the score of 'Stone' by given 'Int'
updateScore :: Game -> Int -> Stone -> Game
updateScore game@(Game m lm s b w status) p st
    | st == Black = (Game m lm s (b+p) w status)
    | otherwise = (Game m lm s b (w+p) status)

-- | 'addKo' adds 'Ko' to the game at given point
addKo :: Game -> (Maybe Point) -> Game
addKo game@(Game m lm s b w status) (Just p) = (Game (addPiece m p Ko) lm s b w status)

-- | 'removeStones' given a list of points removes stones from all those pieces in the game
removeStones :: Game -> [Maybe Point] -> Game
removeStones game@(Game m lm _ _ _ status) [] = game
removeStones game@(Game m lm s b w status) (Nothing:xs) = game
removeStones game@(Game m lm s b w status) ((Just p):xs) = removeStones (Game (removePiece m p) lm s b w status) xs

-- | 'seekBoard' returns the 'Stone' at given 'Point' in the 'Game'
seekBoard :: Game -> Point -> Stone
seekBoard (Game m _ _ _ _ _) p = case Map.lookup p m of
    Just stone -> stone
    Nothing -> Empty

-- | This data type represents whether corresponding stone belongs to Black or White or None
data Status = Seen | Unseen | SeenW | SeenB | None deriving (Eq)

-- | 'seekMap' returns the 'Status' of 'Point' from a given map
seekMap :: (Map Point Status) -> Point -> Status
seekMap m p = case Map.lookup p m of
    Just s -> s
    Nothing -> Unseen

-- | 'findTrappedGroup' finds trapped group of stones in the game
-- starting from point and going in all directions
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

-- | 'findTerritory' finds territory of a given point in the game
-- starting from point and going in all directions
findTerritory :: Game -> Point -> Stone -> ((Map Point Status), [Maybe Point]) -> ((Map Point Status), [Maybe Point])
findTerritory game@(Game _ _ boardSize _ _ _) point@(Point x y) stone (m, points)
    | x < 1 || x > boardSize || y < 1 || y > boardSize = (m, points) -- If point is out of board
    | elem (pure point) points = (m, points) -- If we have already visited this point
    | seekBoard game point == getOppositeStone stone = (m, Nothing:points) -- If this point has opposite stone
    | seekBoard game point == stone = (m, points) -- If this point has current stone
    | otherwise = findTerritory game left stone
        $ findTerritory game right stone
        $ findTerritory game up stone
        $ findTerritory game down stone ((Map.insert point Seen m), ((pure point):points)) -- Otherwise recure on all adjacent points of the stone
    where (up, down, left, right) = getAdj point

-- | 'findTerritories' finds terrirtories of either Black or White stone from given Point
findTerritories :: Game -> Point -> (Map Point Status) -> (Map Point Status)
findTerritories game point m
    | seekBoard game point /= Empty && seekBoard game point /= Ko = addPiece m point None -- If point has Ko or Empty
    | seekMap m point == SeenW = m -- If point belongs to White's territory
    | seekMap m point == SeenB = m -- If point belongs to Black's territory
    | seekMap m point == None = m -- If point has been seen and belongs to none
    | otherwise = if elem Nothing (snd tw) then
                      if elem Nothing (snd tb) then setInMap m (snd tb) None
                      else setInMap m (snd tb) SeenB
                  else setInMap m (snd tw) SeenW
    where tb = findTerritory game point Black (m, [])
          tw = findTerritory game point White (m, [])

-- | 'setInMap' sets staus of points in the map
setInMap :: (Map Point Status) -> [Maybe Point] -> Status -> (Map Point Status)
setInMap m [] st = m
setInMap m (point:points) st | point /= Nothing = setInMap (addPiece m (purify point) st) points st
                             | otherwise = setInMap m points st

-- | 'findAllTerritoriesOfPoints' calulates all the territories of given list of points in Game
findAllTerritoriesOfPoints :: Game -> [Point] -> (Map Point Status) -> (Map Point Status)
findAllTerritoriesOfPoints game [] m = m
findAllTerritoriesOfPoints game (point:points) m = findAllTerritoriesOfPoints game points (findTerritories game point m)

-- | 'findTerritories' creates list of points and finds territories for all the points in the game
findAllTerritories :: Game -> (Map Point Status)
findAllTerritories game@(Game _ _ boardSize _ _ _) = findAllTerritoriesOfPoints game points Map.empty
    where points = [(Point x y) | x <- [1..boardSize], y <- [1..boardSize]]

-- | 'finishGame' finds all the captured territories in the 'Game' and updates the correspoing scores of the 'Stone'
finishGame :: Game -> Game
finishGame game@(Game m lm s sb sw status) = Game m lm s sb' sw' Over
    where sb' = sb + countSeenB t s
          sw' = sw + countSeenW t s
          t = findAllTerritories game

-- | 'countSeenB' counts the points that belong to territory of Black
countSeenB :: (Map Point Status) -> Int -> Int
countSeenB m size | l == size*size = 0 -- If board is empty then it cannot be territory
                  | otherwise = l
                  where l = length $ List.filter (\(p,s) -> s == SeenB) (assocs m)

-- | 'countSeenW' counts the points that belong to territory of White
countSeenW :: (Map Point Status) -> Int -> Int
countSeenW m size | l == size*size = 0 -- If board is empty then it cannot be territory
                  | otherwise = l
                  where l = length $ List.filter (\(p,s) -> s == SeenW) (assocs m)
-- | 'purify' changes Maybe Type to Type
purify :: Maybe a -> a
purify (Just a) = a

-- | 'validMove' checks if the move of given 'Point' to given 'Stone' is valid or not
validMove :: Game -> Point -> Stone -> Bool
validMove game@(Game m lm s _ _ _) p@(Point x y) st | x < 1 || x > s || y < 1 || y > s = False
    | seekBoard game p /= Empty = False -- If point is not empty
    | not $ checkIfTrapped game1 p st = True -- If point is not trapped then validmove
    -- If point is trapped then check if on placing the stone whether opposites stone sare captured or not
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

-- | 'checkIfTrapped' checks whether a stone is trapped or not
checkIfTrapped :: Game -> Point -> Stone -> Bool
checkIfTrapped game p st = not $ elem Nothing (findTrappedGroup game p st [])

-- | 'checkIfNothing' checks whether 'Maybe Point' is 'Nothing'
checkIfNothing :: (Maybe Point) -> Bool
checkIfNothing Nothing = True
checkIfNothing (Just point) = False

-- | 'getWinner' declares the winner depending on the scores of the stones
getWinner :: Game -> String
getWinner game@(Game _ _ _ sb sw _)
    | sb > sw = "Black wins." -- If 'Black' score is more than 'White' score
    | sw > sb = "White wins." -- If 'White' score is more than 'Black' score
    | otherwise = "Game Draws" -- If both the scores are equal
