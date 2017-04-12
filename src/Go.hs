module Go(
    playGo
) where

import BoardGo
import System.IO.Unsafe

playGo :: IO ()
playGo = do
    let game = createGame
    playTurn game Black

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
            let isValidMove = validMove game (Point x y) stone
            if isValidMove then playTurn (playMove (removeKo game) (Point x y) stone) (getOppositeStone stone)
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


getPoints :: Int -> [Point] -> [Point]
getPoints n points = do
    if n == 0 then points
        else do
            let p = unsafePerformIO getPoint
            getPoints (n-1) (p:points)

getPoint :: IO Point
getPoint = do
    x <- getLine
    y <- getLine
    let a = read x
    let b = read y
    return $ Point a b
