import board

main = do
    putStrLn "Hello, World!"

showPossibleMoves :: Int -> [Move] -> String
showPossibleMoves _ [] = ""
showPossibleMoves moveNum moves = foldl str "" moves
