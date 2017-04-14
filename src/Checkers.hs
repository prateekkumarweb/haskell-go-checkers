module Checkers(
    playCheckers
) where

import Data.Map
import BoardCheckers
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | Main function that plays Checkers
playCheckers :: IO()
-- | Play the Game
playCheckers = play window (dark yellow) 0 game render handleEvent (\_ y -> y)
    where -- | The window for 'Game'
          window = InWindow "Checkers Window" (600, 600) (10, 10)
          -- | Get the initial 'BoardMap'
          s = startGame
          -- | Get the starting moves for 'Black'
          moves = validMoves s Black
          -- | Initialize 'Game'
          game = Game s moves Black $ Square (-1) (-1)

-- | Take the 'Game' and return the picture for the "Game"
render :: BoardCheckers.Game -> Picture
-- The final Picture is a collection of blackBoxes, boardBox, greenSquares, blueSquare, pieces, kings
render game@(Game (BoardMap board) moves player (Square x y) ) = pictures [boardBox, (pictures blackboxes), blueSquare, pictures greenSquares, (pictures pieces), (pictures kings), playerBox]
    where -- | Blackboxes of board
          blackboxes = [translate (fromIntegral (2*x -9)*30) (fromIntegral (9-2*y)*30) $ color (greyN 0.25) $ rectangleSolid 60 60 | (Square x y,b) <- assocs board ]
          -- | Pieces on board
          pieces = [translate (fromIntegral (2*x -9)*30) (fromIntegral (9-2*y)*30) $ color c $ circleSolid 24 | (Square x y, Piece pl pt) <- assocs board, c <- [black,red], pl == Black && c == black || pl == Red && c == red]
          -- | Kings on board
          kings = [translate (fromIntegral (2*x -9)*30) (fromIntegral (9-2*y)*30) $ scale 0.2 0.2 $ color white $ text "K" | (Square x y, Piece pl pt) <- assocs board, pt == King]
          -- | Surrounding boardBox
          boardBox = color white $ rectangleSolid 480 480
          -- | Clicked Blue Square
          blueSquare = if x /= (-1) then translate (fromIntegral (2*x -9)*30) (fromIntegral (9-2*y)*30) $ color (light blue) $ rectangleSolid 60 60
                       else translate (fromIntegral (2*x -9)*30) (fromIntegral (9-2*y)*30) $ color (dark yellow) $ rectangleSolid 60 60
          -- | Dest greenSquares
          greenSquares = if x /= (-1) then [ translate (fromIntegral (2*a -9)*30) (fromIntegral (9-2*b)*30) $ color (light green) $ rectangleSolid 60 60 | move <- moves, a <- [1..8], b <- [1..8], getSource move == Square x y, getDestination move == Square a b] else []
          -- | Display turn and log
          playerBox = if length moves == 0 then translate 0 270 $ scale 0.2 0.2 $ text $ "Game Over " ++ (if player == Red then "Black wins" else "Red wins")
                      else translate 0 270 $ scale 0.2 0.2 $ text $ (if player == Red then "Red's turn" else "Black's turn")

-- | Takes an event and 'Game' and returns the board after the result of this event
handleEvent :: Event -> BoardCheckers.Game -> BoardCheckers.Game
-- | When the left mouse button is pressed ..
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game@(Game board moves player _)
    | x' < 1 || x' > 8 || y' < 1 || y' > 8 = game -- If pressed outside board do nothing
    | otherwise = Game board moves player $ Square x' y' -- Otherwise update the clicked 'Square'
    where x' = round $ (x+270)/60 -- Get the x coordinate of the clicked square
          y' = round $ (270-y)/60 -- Get the y coordinate of the clicked square
-- | When the left mouse button is unclicked ..
handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) game@(Game board moves player (Square x' y'))
    | not isValid = game -- If the move to be done is not a valid move do nothing
    | isJump move && length moreJumps > 0 = Game newBoard moreJumps player $ Square (-1) (-1) -- If the player can continue jumping
    | otherwise = Game newBoard nextMoves nextPlayer $ Square (-1) (-1) -- Else the turn goes to the opposite player
    where (move, isValid) = getMoveFromMoves (Square x' y') (Square x'' y'') moves
          x'' = round $ (x+270)/60 -- Get the x coordinate of the clicked square
          y'' = round $ (270-y)/60 -- Get the y coordinate of the clicked square
          newBoard = playMove board move -- Obtain the new Board by playing the move
          moreJumps = mValidJumps newBoard (Square x'' y'', seekBoard newBoard $ Square x'' y'')  -- Get the jumps possible form the dest 'Sqaure'
          nextPlayer = if player == Red then Black else Red -- Get the next Player
          nextMoves = validMoves newBoard nextPlayer -- Get the moves op the next player
handleEvent _ game = game -- Rest of the events - no response

-- | Takes a src and a dest 'Square' and moves and returns if there is a 'Jump' or a 'March' from src to dest
getMoveFromMoves :: Square -> Square -> [Move] -> (Move, Bool)
getMoveFromMoves src dest moves | not foundMove && not foundJump = (Jump src dest, False) -- If not found set False
                                | not foundMove = (Jump src dest, True) -- If 'Jump' found return the 'Jump' and True
                                | otherwise = (March src dest, True) -- If 'March' found return the 'March' and True
                                where foundMove = elem (March src dest) moves
                                      foundJump = elem (Jump src dest) moves
