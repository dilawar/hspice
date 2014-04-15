module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import Parser.Parser
import Text.Parsec.String 

import AST.Network 


-- Main function.
main = do
    result <- parseFromFile jeera "example.cir"
    case result of 
        Left err -> print err
        Right xs -> print xs
    putStrLn "Done"

