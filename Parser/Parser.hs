module Parser.Parser where

import Control.Monad (liftM)
import Text.Parsec
import Parser.Lexer
import Network.Devices
import Parser.Transformer

import Text.Parsec hiding (spaces)

-- The parser
p_jeera = do
    endBy1 p_jeeraStatement (char ';' >> many p_eol)

p_jeeraStatement = 
    p_deviceDeclaration <|> p_connection

p_deviceDeclaration = do
    deviceName <- identifier 
    reservedOp "="
    t <- p_deviceType
    stmts <- (braces $ lexeme p_deviceStatements)
    let device = createDevice deviceName t stmts
    return $ show device

p_deviceStatements = endBy1 p_deviceStatement (char ';' >> skipMany p_eol)

p_deviceStatement =
    (do 
        a <- p_portStatement
        return $ defaultStmt { stmt = a } 
    )
    <|> 
    ( do 
        v <- p_valueStatement 
        return (defaultStmt { stmt = v })
    )

p_portStatement = do
    t <- (( reserved "in" >> return "in" ) <|> (reserved "out" >> return "out"))
    ports <- commaSep1 p_portName 
    return $ createPortExpr t ports 

p_deviceType = do 
    ((reserved "Resistor") >> return "Resistor")
    <|> ((reserved "Capacitor") >> return "Capacitor")
    <|> ((reserved "VSource") >> return "VSource")
    <|> ((reserved "ISource") >> return "ISource")


p_valueStatement = do
    name <- identifier 
    op <- reservedOp "="
    value <- float 
    return $ ValueExpr { vParamName = name, vValue = value }

p_portName = identifier 
    
-- connections
p_connection = identifier 

-- eol
p_eol =  try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"


