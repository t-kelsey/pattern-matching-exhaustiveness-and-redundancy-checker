module Main where

import Parser
import Test

main :: IO ()
main = do putStrLn $ prettyJson $ head exParseJson
