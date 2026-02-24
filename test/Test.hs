module Main where

import Parser
import Test.Hspec 
import Test.QuickCheck
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do  
        describe "Prelude.head" $ do
            -- Parser tests: Test file should be correctly parsed
            it "Parser: data types are correctly parsed" $ do
               contents <- readFile "resources/test.txt"
               let dtypes' = (\(x, _, _) -> x) $ head $ runParserEnd match' contents
               (prettyDType $ head dtypes') `shouldBe` "??"

            it "returns the first element of an *arbitrary* list" $
               property $ \x xs -> head (x:xs) == (x :: Int)

            it "throws an exception if used with an empty list" $ do
               evaluate (head []) `shouldThrow` anyException
