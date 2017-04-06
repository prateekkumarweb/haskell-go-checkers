import BoardGo

main :: IO ()
main = do
    let game = createGame
    playTurn game Black

playTurn :: Game -> Stone -> IO()
playTurn game stone = do
    putStrLn $ show game
    moveX <- getLine
    moveY <- getLine
    let x = read moveX
    let y = read moveY
    let isValidMove = validMove game (Point x y) stone
    if isValidMove then playTurn (playMove game (Point x y) stone) (getOppositeStone stone)
        else playTurn game stone