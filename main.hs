import Board

main = do
    let s = startGame
    let moves = validMoves s Black
    showBoardMovesPlay s moves Black

validateTurn :: BoardMap -> Player -> [Move] -> IO ()
validateTurn board p moves | length moves == 0 = declareWinner p
                           | otherwise = playTurn board p moves

playTurn :: BoardMap -> Player -> [Move] -> IO ()
playTurn board p moves = do
    selectedMoveNum <- (takevalidInput $ length moves)
    let selectedMove = (moves !! ((read selectedMoveNum) - 1))
    let newBoard = playMove board selectedMove
    let destination = getDestination selectedMove
    let moreJumps = mValidJumps newBoard (destination, (seekBoard newBoard destination))
    let nextPlayer = if (p == Red) then Black else Red
    let nextMoves = validMoves newBoard nextPlayer
    if isJump selectedMove && length moreJumps > 0 then showBoardMovesPlay newBoard moreJumps p
        else showBoardMovesPlay newBoard nextMoves nextPlayer

takevalidInput :: Int -> IO String
takevalidInput n = do
    putStrLn "Please select a valid move."
    input <- getLine
    let selectedMoveNum = read input
    if selectedMoveNum <= n && selectedMoveNum >= 0 then return input else takevalidInput n

showBoardMovesPlay :: BoardMap -> [Move] -> Player -> IO()
showBoardMovesPlay board moves p = do
    putStrLn (show board)
    putStrLn (showMoves p moves)
    validateTurn board p moves


showMoves :: Player -> [Move] -> String
showMoves p moves | length moves == 0 = "No moves posssible. " ++ (show p) ++ " loses. \n"
                | otherwise = (show p) ++ "'s move \n" ++ showPossibleMoves 1 moves

showPossibleMoves :: Int -> [Move] -> String
showPossibleMoves _ [] = ""
showPossibleMoves moveNum (x:xs) = show moveNum ++ " " ++ show x ++ showPossibleMoves (moveNum+1) xs

declareWinner :: Player -> IO ()
declareWinner p | p == Red = putStrLn ("Black wins\n")
                | otherwise = putStrLn ("Red wins\n")
