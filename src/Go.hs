module Go(
    playGo
) where

import Data.Map
import BoardGo
import System.IO.Unsafe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

playGo :: IO ()
playGo = do
    let game = createGame
    --let mng = playMove (playMove game (BoardGo.Point 1 2) White) (BoardGo.Point 19 19) Black
    let window = InWindow "Go Window" (20 * 19, 20 * 19) (10, 10)
    play window (dark yellow) 1 game render handleClick f
    --display window (dark yellow) $ render mng
    playTurn game Black

f :: Float -> Game -> Game
f _ game = game

handleClick :: Event -> Game -> Game
handleClick (EventKey (MouseButton LeftButton) _ _ (x, y)) game@(Game _ lm@(Move _ st) s _ _) = if validMove game p $ getOppositeStone st
    then playMove (removeKo game) p $ getOppositeStone st
    else game
    where p = BoardGo.Point (round ((x + 10*(fromIntegral s+1))/20)) (round ((-1*y + 10*(fromIntegral s+1))/20))
handleClick _ game = game

playTurn :: Game -> Stone -> IO()
playTurn game stone = do
    putStrLn $ show game
    putStrLn $ show stone
    putStrLn $ show $ getBlackScore game
    putStrLn $ show $ getWhiteScore game
    moveX <- getLine
    if moveX == "pass"
        then do
            if stone == White && getLastMove game == Pass
                then do
                    putStrLn "GameOver"
                    endGame (playPass game stone)
                else
                    playTurn (playPass game stone) (getOppositeStone stone)
        else do
            moveY <- getLine
            let x = read moveX
            let y = read moveY
            let isValidMove = validMove game (BoardGo.Point x y) stone
            if isValidMove then playTurn (playMove (removeKo game) (BoardGo.Point x y) stone) (getOppositeStone stone)
                else playTurn game stone

endGame :: Game -> IO ()
endGame game@(Game m lm size sb sw) = do
    putStrLn "Enter the hopeless points"
    n <- getLine
    let hs = getPoints (read n) []
    print hs
    let newg = removeHopeless game $ hs
    putStrLn $ show game
    showWinner newg
    putStrLn $ show $ findAllTerritories newg

showWinner :: Game -> IO ()
showWinner newg = do
    putStrLn $ getWinner newg
    putStrLn $ show $ getBlackScore newg
    putStrLn $ show $ getWhiteScore newg
    putStrLn $ show newg


getPoints :: Int -> [BoardGo.Point] -> [BoardGo.Point]
getPoints n points = do
    if n == 0 then points
        else do
            let p = unsafePerformIO getPoint
            getPoints (n-1) (p:points)

getPoint :: IO BoardGo.Point
getPoint = do
    x <- getLine
    y <- getLine
    let a = read x
    let b = read y
    return $ BoardGo.Point a b

render :: Game -> Picture
render game@(Game m _ s _ _ ) = pictures [ (pictures gameHorizontalLines), (pictures gameVerticalLines), (pictures stones) ]
    --where gameHorizontalLines = [line [(-180,0)
    where gameHorizontalLines = [line [(fromIntegral (10 - 10*s),fromIntegral (10*s - 10 - 20*x)), (fromIntegral (20*s - 10 - 10*s),fromIntegral (10*s - 10 - 20*x))] | x <- [0..(s-1)]]
          gameVerticalLines = [line [(fromIntegral (10*s - 10 - 20*x),fromIntegral (10 - 10*s)), (fromIntegral (10*s - 10 - 20*x),fromIntegral (-10*s + 20*s - 10))] | x <- [0..(s-1)]]
          stones = [translate (fromIntegral (x*20 - 10*s-10)) (fromIntegral (10*s - y*20 + 10)) $ color c $ circleSolid 8 | (BoardGo.Point x y, st) <- (assocs m) , c <- [black,white], ((st == Black && c == black) ||  (st == White && c == white))]
