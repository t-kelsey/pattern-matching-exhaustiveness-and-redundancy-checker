module Main where

import Parser

main :: IO ()
main = do
    contents <- readFile "resources/test.txt"
    case runParserEnd dtypefunctions contents of
        []    -> error "\n\nParser Error: runParserEnd could not match test.txt. Check your syntax.\n"
        (x:_) -> putStrLn "\nsuccess\n"
        --(x:_) -> putStrLn (prettyDTypes x)

