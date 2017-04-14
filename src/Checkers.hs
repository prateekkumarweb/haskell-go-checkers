module Checkers(
    playCheckers
) where

import Data.Map
import BoardCheckers
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

playCheckers :: IO()
playCheckers = do
    let window = InWindow "Checkers Window" (800, 800) (100, 100)
    let s = startGame
    let moves = validMoves s Black
    --showBoardMovesPlay s moves Black
    let game = Game s moves Black $ Square (-1) (-1)
    play window (dark yellow) 0 game render handleEvent (\_ y -> y)

render :: BoardCheckers.Game -> Picture
render game@(Game (BoardMap board) moves player (Square x y) ) = pictures [boardBox, (pictures blackboxes), blueSquare, pictures greenSquares, (pictures pieces), (pictures kings), playerBox]
    where blackboxes = [translate (fromIntegral (2*x -9)*40) (fromIntegral (9-2*y)*40) $ color (greyN 0.25) $ rectangleSolid 80 80 | (Square x y,b) <- assocs board ]
          pieces = [translate (fromIntegral (2*x -9)*40) (fromIntegral (9-2*y)*40) $ color c $ circleSolid 30 | (Square x y, Piece pl pt) <- assocs board, c <- [black,red], pl == Black && c == black || pl == Red && c == red]
          kings = [translate (fromIntegral (2*x -9)*40) (fromIntegral (9-2*y)*40) $ scale 0.2 0.2 $ color white $ text "K" | (Square x y, Piece pl pt) <- assocs board, pt == King]
          boardBox = color white $ rectangleSolid 640 640
          blueSquare = if x /= (-1) then translate (fromIntegral (2*x -9)*40) (fromIntegral (9-2*y)*40) $ color (light blue) $ rectangleSolid 80 80
                       else translate (fromIntegral (2*x -9)*40) (fromIntegral (9-2*y)*40) $ color (dark yellow) $ rectangleSolid 80 80
          greenSquares = if x /= (-1) then [ translate (fromIntegral (2*a -9)*40) (fromIntegral (9-2*b)*40) $ color (light green) $ rectangleSolid 80 80 | move <- moves, a <- [1..8], b <- [1..8], getSource move == Square x y, getDestination move == Square a b] else []
          playerBox = if length moves == 0 then translate (-180) 360 $ scale 0.2 0.2 $ text $ "Game Over " ++ (if player == Red then "Black wins" else "Red wins")
                      else translate 0 360 $ scale 0.2 0.2 $ text $ (if player == Red then "Red's turn" else "Black's turn")

handleEvent :: Event -> BoardCheckers.Game -> BoardCheckers.Game
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game@(Game board moves player _)
    | x' < 1 || x' > 8 || y' < 1 || y' > 8 = game
    | otherwise = Game board moves player $ Square x' y'
    where x' = round $ (x+360)/80
          y' = round $ (360-y)/80
handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) game@(Game board moves player (Square x' y'))
    | not isValid = game
    | isJump move && length moreJumps > 0 = Game newBoard moreJumps player $ Square (-1) (-1)
    | otherwise = Game newBoard nextMoves nextPlayer $ Square (-1) (-1)
    where (move, isValid) = getMoveFromMoves (Square x' y') (Square x'' y'') moves
          x'' = round $ (x+360)/80
          y'' = round $ (360-y)/80
          newBoard = playMove board move
          moreJumps = mValidJumps newBoard (Square x'' y'', seekBoard newBoard $ Square x'' y'')
          nextPlayer = if player == Red then Black else Red
          nextMoves = validMoves newBoard nextPlayer
handleEvent _ game = game

getMoveFromMoves :: Square -> Square -> [Move] -> (Move, Bool)
getMoveFromMoves src dest moves | not foundMove && not foundJump = (Jump src dest, False)
                                | not foundMove = (Jump src dest, True)
                                | otherwise = (March src dest, True)
                                where foundMove = elem (March src dest) moves
                                      foundJump = elem (Jump src dest) moves

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
