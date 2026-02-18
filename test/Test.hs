module Main where

import Parser
import Test.Hspec 
import Test.QuickCheck
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do  
        describe "Prelude.head" $ do
            it "returns the first element of a list" $ do
               contents <- readFile "resources/test.txt"
               putStrLn $ runParserEnd match' contents

            it "returns the first element of an *arbitrary* list" $
               property $ \x xs -> head (x:xs) == (x :: Int)

            it "throws an exception if used with an empty list" $ do
               evaluate (head []) `shouldThrow` anyException