module Main where

import Parser
import Detection
import Data.List (intercalate, transpose)

main :: IO ()
main = do
    contents <- readFile "resources/input.txt"
    case findParseError contents of

        (Right ())  -> -- No parse errors detected
            case runParserEnd match' contents of
            
            []      -> putStrLn "\n\n  Parse failure: Could not match input.txt. Check your syntax.\n"

            -- If the datatypes and pattern matrix parse, run the warnings function on them
            ((dts, pm):_) -> putStrLn (warnings dts pm)
            

        (Left errorText) -> putStrLn errorText



-- This is the main function of this project. The output is the combination of warnings 
-- generated, semantic input checking, and of course the exhaustiveness check.
warnings :: DTypes -> PMat -> String
warnings dts pm = case typeCheck dts pm of

        (Right ()) -> do
            let header = "\n\n--- EXHAUSTIVENESS AND REDUNDANCY CHECKER OUTPUT ---"
            let footer = "\n\n----------------------------------------------------"

            header ++ isExTextGen ++ vbTextGen ++ urTextGen ++ footer

        (Left errorText) -> errorText

    where 
          -- Returns the text for checking exhaustiveness
          isExTextGen :: String
          isExTextGen =
            case pm `isExhaustiveUnder` dts of

                True -> "\n\n    Success: Cases are exhaustive"
                False -> "\n\n    Failure: Cases are not exhaustive" ++ casesForExTextGen

          -- Returns the text for how each variable was bound
          vbTextGen :: String 
          vbTextGen = "\n\n\n    with bindings:  \n\n        column 1                      column 2                      ... \n      "  
                      ++ intercalate "\n      " (formatRow <$> transpose (getVarBindings dts pm)) 


          formatRow :: [[(String, Maybe Type)]] -> String 
          formatRow [] = "" 
          formatRow cells = concat ((\s -> s ++ replicate (30 - length s) ' ') . formatCell <$> cells) 

          formatCell [] = " [ --- ] " 
          formatCell binds = " [ " ++ intercalate ", " (formatBind <$> binds) ++ " ] " 

          formatBind (v, Just t)  = v ++ ": " ++ t 
          formatBind (v, Nothing) = v ++ ": ?"

          convertVarBindsToString :: [(String, Maybe Type)] -> String
          convertVarBindsToString [] = ""
          convertVarBindsToString ((v, mt):xs) = 
            case mt of
                Just t  -> v ++ ": " ++ t ++ ",     " ++ convertVarBindsToString xs
                Nothing -> v ++ ": ?"     ++ ",       " ++ convertVarBindsToString xs

          -- Returns the text for redundant cases, also called useless rows
          urTextGen :: String
          urTextGen =
            case containsUselessRow dts pm of

                (Just pv) -> "\n\n    '" ++ prettyPVec pv ++ "'\n    ^ This case in the pattern matrix is redundant"
                Nothing -> "\n\n    No cases are redundant."

          casesForExTextGen :: String
          casesForExTextGen = "\n\n    Maybe you forgot this case: " ++ case witness dts pm (length $ head pm) of
                                                                            (Left ()) -> "Could not find missing case."
                                                                            (Right pv) -> prettyPVec pv


findParseError :: String -> Either String ()
findParseError s = do

    checkSectionHeaders s -- Ensure the dividers of the sections are correctly in place
    checkEmptySections s  -- Ensure each section isn't empty
    checkDTypes s         -- Ensure no data types are malfomed 
    checkPMat s           -- Ensure the pattern matrix is not malformed
    -- checkVVec s           -- Ensure the types (values) given aren't malformed. Currently not needed as column type is implicit!

-- TO-DO: 
-- tests
-- check bound variables again
-- X display how each var is bound
-- X redundancy 
-- X delete type (last section in input)
