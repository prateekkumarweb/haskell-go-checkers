module Main(main) where

import Go
import Checkers

main :: IO()
main = do
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
