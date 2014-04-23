module Parser.Parser where

import Control.Applicative hiding ((<|>))
import Text.Parsec
import Parser.Lexer
import Network.Devices
import Parser.Transformer
import Text.Parsec.Prim

import Text.Parsec hiding (spaces)

data Sign = Positive | Negative deriving (Eq, Show)

-- The parser
p_jeera = do
    design <- many1 p_jeeraStatement
    return design

p_jeeraStatement = 
    (do
        deviceDecl <- p_deviceDeclaration
        return deviceDecl
    ) 
    <|> (do
        connection <- p_connection 
        return connection
    )

p_deviceDeclaration = do
    deviceName <- identifier 
    reservedOp "="
    t <- p_deviceType
    stmts <- braces p_deviceStatements 
    semi
    let device = createDevice deviceName t stmts
    return device

p_deviceStatements = do 
    endBy1 p_deviceStatement semi

p_deviceStatement =
    p_portStatement <|> p_valueStatement <|> p_initValue <|> p_parameter

p_deviceType = do 
    ((reserved "Resistor") >> return "Resistor")
    <|> ((reserved "Capacitor") >> return "Capacitor")
    <|> ((reserved "VSource") >> return "VSource")
    <|> ((reserved "ISource") >> return "ISource")

p_portStatement = do
    t <- (( reserved "in" >> return "in" ) <|> (reserved "out" >> return "out"))
    ports <- commaSep p_portName 
    return $ createPortExpr t ports 


p_valueStatement = do
    (reserved "value") 
    (reservedOp "=")
    value <- p_signedFloatOrInteger 
    return $ ValueExpr { vValue = value }

p_initValue = do
    (reserved "init")
    (reservedOp "=")
    value <- p_signedFloatOrInteger 
    return $ InitExpr { initValue = value }

p_parameter = do
    pname <- identifier 
    (reservedOp "=")
    value <- p_signedFloatOrInteger 
    return $ ParamExpr { pName = pname, pValue = value }

p_connection = do
    identifier 

p_signedFloatOrInteger = 
    (do 
        (char '-') 
        f <- p_floatOrInteger
        return (0.0 - f)
    )
    <|> (do 
        (char '+') 
        f <- p_floatOrInteger 
        return f
    )
    <|> (do 
        f <- p_floatOrInteger 
        return f
    )


p_floatOrInteger =
    try (float >>= return)
    <|> 
    try (integer >>= return . fromIntegral)
    <|>
    (do  
        num <- p_floatOrInteger
        (char 'e')
        p <- integer
        return $ num * (10 ^^ p)
    )

-- Port names 
p_portName = identifier 
