module Parser.Parser where

import Control.Monad (liftM)
import Text.Parsec
import Parser.Lexer
import Network.Devices
import Parser.Transformer

import Text.Parsec hiding (spaces)

-- The parser
p_jeera = do
    many1 p_jeeraStatement

p_jeeraStatement = 
    p_deviceDeclaration -- <|> p_connection

p_deviceDeclaration = do
    deviceName <- identifier 
    reservedOp "="
    t <- p_deviceType
    stmts <- braces p_deviceStatements 
    semi
    let device = createDevice deviceName t stmts
    return $ show stmts

p_deviceStatements = do 
    endBy1 p_deviceStatement semi

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

p_deviceType = do 
    ((reserved "Resistor") >> return "Resistor")
    <|> ((reserved "Capacitor") >> return "Capacitor")
    <|> ((reserved "VSource") >> return "VSource")
    <|> ((reserved "ISource") >> return "ISource")
    <|> ((reserved "Device") >> return "Device")


p_portStatement = do
    t <- (( reserved "in" >> return "in" ) <|> (reserved "out" >> return "out"))
    ports <- commaSep p_portName 
    return $ createPortExpr t ports 


p_valueStatement = do
    (reserved "value") 
    (reservedOp "=")
    value <- p_floatOrInteger 
    return $ ValueExpr { vValue = value }

p_floatOrInteger =  ((float >>= return) <|> (integer >>= return . fromInteger))

-- Port names 
p_portName = identifier 
    
-- connections
{-p_connection = identifier -}


