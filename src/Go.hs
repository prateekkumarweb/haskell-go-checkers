module Go(
    playGo
) where

import Data.Map
import BoardGo
-- import System.IO.Unsafe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

playGo :: IO ()
playGo = do
    putStrLn "Please select 1 of the sizes - 9 13 or 19"
    sizeString <- getLine
    let size = read sizeString
    let game = createGame size
    let window = InWindow "Go Window" (20 * size + 240, 20 * size) (10, 10)
    play window (dark yellow) 0 game render handleEvent (\_ y -> y)


handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game@(Game _ lm s _ _ Alive)
    | validMove game p $ getOppositeStone st = playMove (removeKo game) p st
    where p = BoardGo.Point (round ((x + 10*(fromIntegral s+1))/20)) (round ((-1*y + 10*(fromIntegral s+1))/20))
          st = getOppositeStoneFromLastMove lm
handleEvent (EventKey (Char 'p') Down _ _) game@(Game _ lm s _ _ Alive)
    | getLastMove game == Pass Black = killGame game
    | otherwise = playPass (removeKo game) st
    where st = getOppositeStoneFromLastMove lm
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game@(Game m lm s sb sw Dead) = Game (removePiece m (BoardGo.Point x' y')) lm s sb' sw' Dead
    where x' = round ((x + 10*(fromIntegral s+1))/20)
          y' = round ((-1*y + 10*(fromIntegral s+1))/20)
          sb' = if seekBoard game (BoardGo.Point x' y') == White then sb+1 else sb
          sw' = if seekBoard game (BoardGo.Point x' y') == Black then sw+1 else sw
handleEvent (EventKey (Char 'e') Down _ _) game@(Game _ lm s _ _ Dead) = finishGame game
handleEvent _ game = game

getOppositeStoneFromLastMove :: Move -> Stone
getOppositeStoneFromLastMove (Move _ st) = getOppositeStone st
getOppositeStoneFromLastMove (Pass st) = getOppositeStone st


-- playTurn :: Game -> Stone -> IO()
-- playTurn game stone = do
--     putStrLn $ show game
--     putStrLn $ show stone
--     putStrLn $ show $ getBlackScore game
--     putStrLn $ show $ getWhiteScore game
--     moveX <- getLine
--     if moveX == "pass"
--         then do
--             if stone == White && getLastMove game == Pass
--                 then do
--                     putStrLn "GameOver"
--                     endGame (playPass game stone)
--                 else
--                     playTurn (playPass game stone) (getOppositeStone stone)
--         else do
--             moveY <- getLine
--             let x = read moveX
--             let y = read moveY
--             let isValidMove = validMove game (BoardGo.Point x y) stone
--             if isValidMove then playTurn (playMove (removeKo game) (BoardGo.Point x y) stone) (getOppositeStone stone)
--                 else playTurn game stone

-- endGame :: Game -> IO ()
-- endGame game@(Game m lm size sb sw status) = do
--     putStrLn "Enter the hopeless points"
--     n <- getLine
--     let hs = getPoints (read n) []
--     print hs
--     let newg = removeHopeless game $ hs
--     --putStrLn $ show game
--     showWinner newg
--     putStrLn $ show $ findAllTerritories newg
--
-- showWinner :: Game -> IO ()
-- showWinner newg = do
--     putStrLn $ getWinner newg
--     putStrLn $ show $ getBlackScore newg
--     putStrLn $ show $ getWhiteScore newg
--     --putStrLn $ show newg
--
--
-- getPoints :: Int -> [BoardGo.Point] -> [BoardGo.Point]
-- getPoints n points = do
--     if n == 0 then points
--         else do
--             let p = unsafePerformIO getPoint
--             getPoints (n-1) (p:points)
--
-- getPoint :: IO BoardGo.Point
-- getPoint = do
--     x <- getLine
--     y <- getLine
--     let a = read x
--     let b = read y
--     return $ BoardGo.Point a b

render :: Game -> Picture
render game@(Game m _ s sb sw _) = pictures [ (pictures gameHorizontalLines), (pictures gameVerticalLines), (pictures stones), passButton, scoreBlack]
    --where gameHorizontalLines = [line [(-180,0)
    where gameHorizontalLines = [line [(fromIntegral (10 - 10*s),fromIntegral (10*s - 10 - 20*x)), (fromIntegral (20*s - 10 - 10*s),fromIntegral (10*s - 10 - 20*x))] | x <- [0..(s-1)]]
          gameVerticalLines = [line [(fromIntegral (10*s - 10 - 20*x),fromIntegral (10 - 10*s)), (fromIntegral (10*s - 10 - 20*x),fromIntegral (-10*s + 20*s - 10))] | x <- [0..(s-1)]]
          stones = [translate (fromIntegral (x*20 - 10*s-10)) (fromIntegral (10*s - y*20 + 10)) $ color c $ circleSolid 8 | (BoardGo.Point x y, st) <- (assocs m) , c <- [black,white], ((st == Black && c == black) ||  (st == White && c == white))]
          passButton = translate (fromIntegral (10*s + 60)) (fromIntegral (10*s - 60)) $ color red $ rectangleSolid 80 40
          scoreBlack = translate (fromIntegral (10*s + 20)) (fromIntegral (10*s - 60)) $ color white $ scale 0.1 0.1 $ text $ "Black : " ++ (show sb) ++ "\n" ++ " White" ++ (show sw)
