module Checkers(
    playCheckers
) where

import Data.Map
import BoardCheckers
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

playCheckers :: IO()
playCheckers = do
    let window = InWindow "Checkers Window" (600, 600) (10, 10)
    let s = startGame
    let moves = validMoves s Black
    let game = Game s moves Black $ Square (-1) (-1)
    play window (dark yellow) 0 game render handleEvent (\_ y -> y)

render :: BoardCheckers.Game -> Picture
render game@(Game (BoardMap board) moves player (Square x y) ) = pictures [boardBox, (pictures blackboxes), blueSquare, pictures greenSquares, (pictures pieces), (pictures kings), playerBox]
    where blackboxes = [translate (fromIntegral (2*x -9)*30) (fromIntegral (9-2*y)*30) $ color (greyN 0.25) $ rectangleSolid 60 60 | (Square x y,b) <- assocs board ]
          pieces = [translate (fromIntegral (2*x -9)*30) (fromIntegral (9-2*y)*30) $ color c $ circleSolid 24 | (Square x y, Piece pl pt) <- assocs board, c <- [black,red], pl == Black && c == black || pl == Red && c == red]
          kings = [translate (fromIntegral (2*x -9)*30) (fromIntegral (9-2*y)*30) $ scale 0.2 0.2 $ color white $ text "K" | (Square x y, Piece pl pt) <- assocs board, pt == King]
          boardBox = color white $ rectangleSolid 480 480
          blueSquare = if x /= (-1) then translate (fromIntegral (2*x -9)*30) (fromIntegral (9-2*y)*30) $ color (light blue) $ rectangleSolid 60 60
                       else translate (fromIntegral (2*x -9)*30) (fromIntegral (9-2*y)*30) $ color (dark yellow) $ rectangleSolid 60 60
          greenSquares = if x /= (-1) then [ translate (fromIntegral (2*a -9)*30) (fromIntegral (9-2*b)*30) $ color (light green) $ rectangleSolid 60 60 | move <- moves, a <- [1..8], b <- [1..8], getSource move == Square x y, getDestination move == Square a b] else []
          playerBox = if length moves == 0 then translate 0 270 $ scale 0.2 0.2 $ text $ "Game Over " ++ (if player == Red then "Black wins" else "Red wins")
                      else translate 0 270 $ scale 0.2 0.2 $ text $ (if player == Red then "Red's turn" else "Black's turn")


handleEvent :: Event -> BoardCheckers.Game -> BoardCheckers.Game
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game@(Game board moves player _)
    | x' < 1 || x' > 8 || y' < 1 || y' > 8 = game
    | otherwise = Game board moves player $ Square x' y'
    where x' = round $ (x+270)/60
          y' = round $ (270-y)/60
handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) game@(Game board moves player (Square x' y'))
    | not isValid = game
    | isJump move && length moreJumps > 0 = Game newBoard moreJumps player $ Square (-1) (-1)
    | otherwise = Game newBoard nextMoves nextPlayer $ Square (-1) (-1)
    where (move, isValid) = getMoveFromMoves (Square x' y') (Square x'' y'') moves
          x'' = round $ (x+270)/60
          y'' = round $ (270-y)/60
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
