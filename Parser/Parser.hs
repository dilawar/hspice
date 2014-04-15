module Parser.Parser where

import Control.Monad (liftM)
import Text.Parsec
import Parser.Lexer
import Text.Parsec.String 

import Text.Parsec hiding (spaces)

-- The parser
jeera = endBy1 jeeraStatement (char ';' >> many eol)

jeeraStatement = 
    deviceDeclaration <|> connection

deviceDeclaration = do
    deviceName <- identifier 
    reservedOp "="
    t <- deviceType
    dstmts <- (braces $ lexeme deviceStatements)
    return "Device"

deviceStatements = endBy1 deviceStatement (char ';' >> skipMany eol)

deviceStatement =  do
    portStatement <|> valueStatement 

portStatement = do
    t <- (reserved "in") <|> (reserved "out" ) 
    ports <- commaSep1 portName 
    return "portStatement"

deviceType = do 
    (reserved "Resistor")
    <|> (reserved "Capacitor")
    <|> (reserved "VSource")
    <|> (reserved "ISource")



valueStatement = do
    name <- identifier 
    op <- reservedOp "="
    value <- decimal 
    return "Value"

portName = identifier 
    
-- connections
connection = identifier 

-- eol
eol =  try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

-- Main function.
main = do
    result <- parseFromFile jeera "example.cir"
    case result of 
        Left err -> print err
        Right xs -> print xs
    putStrLn "Done"
