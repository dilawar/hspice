module Parser.Lexer where 

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)


jeeraDef = emptyDef {
        P.commentStart   = "/*"
        , P.commentEnd     = "*/"
        , P.commentLine    = "//"
        , P.nestedComments = True
        , P.identStart     = letter <|> char '_'
        , P.identLetter    = alphaNum <|> oneOf "_'"
        , P.opStart        = P.opLetter emptyDef
        , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , P.reservedOpNames= ["=", "||", "--", "-|", "|-" ]
        , P.reservedNames  = ["Resistor", "Capacitor", "Inductor"
                            , "VSource", "ISource"
                            , "in", "out"
                            ]
        , P.caseSensitive  = True
        }
        
lexer = P.makeTokenParser jeeraDef

identifier = P.identifier lexer
reserved = P.reserved lexer
operator = P.operator lexer
reservedOp = P.reservedOp lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer
natural = P.natural lexer
integer = P.integer lexer
float = P.float lexer
naturalOrFloat = P.naturalOrFloat lexer
decimal = P.decimal lexer
hexadecimal = P.hexadecimal lexer
octal = P.octal lexer
symbol = P.symbol lexer
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
braces = P.braces lexer
angles = P.angles lexer
brackets = P.brackets lexer
semi = P.semi lexer
comma = P.comma lexer
colon = P.colon lexer
dot = P.dot lexer
semiSep = P.semiSep lexer
semiSep1 = P.semiSep1 lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer