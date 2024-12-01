module Main where

import Lib

main :: IO ()
main = do
    putStrLn "\nВведите название файла:"
    filename <- getLine
    content <- readFile $ filename ++ ".txt"
   -- print content
    putStrLn ""
    messymain content
    putStrLn ""
