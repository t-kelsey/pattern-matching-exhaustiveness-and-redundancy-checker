module Main where

import Parser

main :: IO ()
main = do
    contents <- readFile "resources/test.txt"
    case runParserEnd match' contents of
        []    -> error "\n\nParser Error: runParserEnd could not match input.txt. Check your syntax.\n"
        (x:_) -> putStrLn (prettyMatch x)
