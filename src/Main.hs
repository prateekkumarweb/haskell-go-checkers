module Main(main) where

import Go
import Checkers
import Graphics.Gloss

main :: IO()
main = do
    --display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
    putStrLn $ "Enter 1 to play Go"
    putStrLn $ "Enter 2 to play Checkers"
    putStrLn $ "Enter 0 to Exit"
    x <- getLine
    let i = read x
    if i == 1 then playGo else
        if i == 2 then playCheckers else
            if i == 0 then putStrLn ("Thank you") else do
            putStrLn $ "Invalid Input"
            main
