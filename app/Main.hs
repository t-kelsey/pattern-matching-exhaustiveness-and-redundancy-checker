module Main where

import Parser
import UsefulClause
import Detection

main :: IO ()
main = do
    contents <- readFile "resources/input.txt"
    case runParserEnd match' contents of
        []    -> error "\n\nParser Error: runParserEnd could not match test.txt. Check your syntax.\n"
        --(x:_) -> putStrLn "\nsuccess\n"
        ((dts, p, _):_) -> putStrLn (show $ isExhaustive dts p)


