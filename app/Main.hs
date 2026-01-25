module Main where

import Parser

main :: IO ()
main = do putStrLn $ prettyJson $ head exParseJson
