module Main(main) where

import Lib

main :: IO ()
main =
    putStrLn "\nВведите название файла:" >>
    getLine >>= \filename ->
    readFile (filename ++ ".txt") >>= \content ->
    print content >>
    putStrLn "" >>
    messymain content >>
    putStrLn ""
