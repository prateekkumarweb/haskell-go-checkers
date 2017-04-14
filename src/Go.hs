module Go(
    playGo
) where

import Data.Map
import BoardGo
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

playGo :: IO ()
playGo = do
    putStrLn "Please enter the size of the board - 9 13 or 19"
    sizeString <- getLine
    let size = read sizeString
    if size /= 9 && size /= 13 && size /= 19 then playGo else do
        let game = createGame size
        let window = InWindow "Go Window" (20 * size + 180, 20 * size +240) (10, 10)
        play window (dark yellow) 0 game render handleEvent (\_ y -> y)

handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game@(Game _ lm s _ _ Alive)
    | validMove game p st = playMove (removeKo game) p st
    where p = BoardGo.Point (round ((x + 10*(fromIntegral s+1))/20)) (round ((-1*y + 10*(fromIntegral s+1))/20))
          st = getOppositeStoneFromLastMove lm
handleEvent (EventKey (Char 'p') Down _ _) game@(Game _ lm s _ _ Alive)
    | lm == Pass Black = killGame game
    | otherwise = playPass (removeKo game) st
    where st = getOppositeStoneFromLastMove lm
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game@(Game m lm s sb sw Dead) = Game (removePiece m (BoardGo.Point x' y')) lm s sb' sw' Dead
    where x' = round ((x + 10*(fromIntegral s+1))/20)
          y' = round ((-1*y + 10*(fromIntegral s+1))/20)
          sb' = if seekBoard game (BoardGo.Point x' y') == White then sb+1 else sb
          sw' = if seekBoard game (BoardGo.Point x' y') == Black then sw+1 else sw
handleEvent (EventKey (Char 'e') Down _ _) game@(Game _ lm s _ _ Dead) = finishGame game
handleEvent _ game = game

getOppositeStoneFromLastMove :: Move -> Stone
getOppositeStoneFromLastMove (Move _ st) = getOppositeStone st
getOppositeStoneFromLastMove (Pass st) = getOppositeStone st

render :: Game -> Picture
render game@(Game m lm s sb sw status) = pictures [ (pictures gameHorizontalLines), (pictures gameVerticalLines), (pictures stones),  scoreBlack, scoreWhite, statusText]
    where gameHorizontalLines = [line [(fromIntegral (10 - 10*s),fromIntegral (10*s - 10 - 20*x)), (fromIntegral (20*s - 10 - 10*s),fromIntegral (10*s - 10 - 20*x))] | x <- [0..(s-1)]]
          gameVerticalLines = [line [(fromIntegral (10*s - 10 - 20*x),fromIntegral (10 - 10*s)), (fromIntegral (10*s - 10 - 20*x),fromIntegral (-10*s + 20*s - 10))] | x <- [0..(s-1)]]
          stones = [translate (fromIntegral (x*20 - 10*s-10)) (fromIntegral (10*s - y*20 + 10)) $ color c $ circleSolid 8 | (BoardGo.Point x y, st) <- (assocs m) , c <- [black,white], ((st == Black && c == black) ||  (st == White && c == white))]
          scoreBlack = translate 0 (fromIntegral (10*s + 40)) $ scale 0.1 0.1 $ text $ "Black's Score : " ++ (show sb)
          scoreWhite = translate 0 (fromIntegral (10*s + 60)) $ scale 0.1 0.1 $ text $ "White's Score : " ++ (show sw)
          statusText = if status == Alive then translate (fromIntegral (-10*s)) (fromIntegral (-10*s - 60)) $ scale 0.2 0.2 $ text $ (show $ getOppositeStone (getPiece lm)) ++ "'s turn"
                       else if status == Dead then translate (fromIntegral (-10*s)) (fromIntegral (-10*s - 60)) $ scale 0.2 0.2 $ text "Click on hopeless stones and enter e"
                       else translate (fromIntegral (-10*s)) (fromIntegral (-10*s - 60)) $ scale 0.2 0.2 $ text $ getWinner game
