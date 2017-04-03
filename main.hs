import board

main = do
    putStrLn "Hello, World!"

playTurn :: BoardMap -> Player -> IO ()
playTurn board p = do
    let moves = validMoves board p
    putStrLn ("Choose a move ")
    putStrLn (showMoves moves)
    -- if length moves == 0 game over you lose
    selectedMoveNum <- getLine
    --if invalid
    let selectedMove = (moves !! ((read selectedMoveNum) - 1))
    let newBoard = playMove board selectedMove
    let destination = getDestination selectedMove
    -- mValidJumps board destination (seekBoard  destination) > 0
    -- -- playTurn




showMoves :: [Move] -> String
showMoves moves | length moves == 0 = "No moves posssible. Player loses."
                | otherwise showPossibleMoves 1 moves

showPossibleMoves :: Int -> [Move] -> String
showPossibleMoves _ [] = ""
showPossibleMoves moveNum (x:xs) = show moveNum ++ " " ++ show x ++ showPossibleMoves (moveNum+1) xs
