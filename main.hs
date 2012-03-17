module Main where
import System.Environment

main::IO()
main = do
    args <- getArgs
    putStrLn("Enter your name please")
    line <- getLine
    putStrLn(line)
