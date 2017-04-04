import Board

main = do
    let s = startGame
    let moves = validMoves s Black
    showBoardMovesPlay s moves Black

playTurn :: BoardMap -> Player -> [Move] -> IO ()
playTurn board p moves = do
    -- if length moves == 0 game over you lose
    selectedMoveNum <- getLine
    --if invalid
    let selectedMove = (moves !! ((read selectedMoveNum) - 1))
    let newBoard = playMove board selectedMove
    let destination = getDestination selectedMove
    let moreJumps = mValidJumps newBoard (destination, (seekBoard newBoard destination))
    let nextPlayer = if (p == Red) then Black else Red
    let nextMoves = validMoves newBoard nextPlayer
    if isJump selectedMove && length moreJumps > 0 then showBoardMovesPlay newBoard moreJumps p
        else showBoardMovesPlay newBoard nextMoves nextPlayer
        -- then
        --     putStrLn (show newBoard)
        --     putStrLn (showMoves moreJumps)
        --     playTurn newBoard p moreJumps
        -- else
        --     putStrLn (show newBoard)
        --     putStrLn (showMoves nextMoves)
        --     playTurn newBoard nextPlayer nextMoves
    -- -- playTurn

showBoardMovesPlay :: BoardMap -> [Move] -> Player -> IO()
showBoardMovesPlay board moves p = do
    putStrLn (show board)
    putStrLn (showMoves moves)
    playTurn board p moves


showMoves :: [Move] -> String
showMoves moves | length moves == 0 = "No moves posssible. Player loses."
                | otherwise = showPossibleMoves 1 moves

showPossibleMoves :: Int -> [Move] -> String
showPossibleMoves _ [] = ""
showPossibleMoves moveNum (x:xs) = show moveNum ++ " " ++ show x ++ showPossibleMoves (moveNum+1) xs
