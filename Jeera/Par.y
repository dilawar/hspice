-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Jeera.Par where
import Jeera.Abs
import Jeera.Lex
import Jeera.ErrM

}

%name pProgram Program

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '(' { PT _ (TS _ 1) }
 ')' { PT _ (TS _ 2) }
 '*' { PT _ (TS _ 3) }
 '+' { PT _ (TS _ 4) }
 ',' { PT _ (TS _ 5) }
 '-' { PT _ (TS _ 6) }
 '/' { PT _ (TS _ 7) }
 ';' { PT _ (TS _ 8) }
 '=' { PT _ (TS _ 9) }
 'Capacitor' { PT _ (TS _ 10) }
 'Device' { PT _ (TS _ 11) }
 'Inductor' { PT _ (TS _ 12) }
 'Resistor' { PT _ (TS _ 13) }
 'input' { PT _ (TS _ 14) }
 'output' { PT _ (TS _ 15) }
 'value' { PT _ (TS _ 16) }
 '{' { PT _ (TS _ 17) }
 '}' { PT _ (TS _ 18) }

L_ident  { PT _ (TV $$) }
L_doubl  { PT _ (TD $$) }
L_integ  { PT _ (TI $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }

Program :: { Program }
Program : ListStatement { Program $1 } 


ListStatement :: { [Statement] }
ListStatement : Statement ';' { (:[]) $1 } 
  | Statement ';' ListStatement { (:) $1 $3 }


Statement :: { Statement }
Statement : DeviceDecl { DeviceDecl $1 } 


DeviceDecl :: { DeviceDecl }
DeviceDecl : InstanceName '=' SimpleDeviceType '{' ListDeviceStatement '}' { SimpleDevice $1 $3 $5 } 
  | InstanceName '=' 'Device' '{' ListDeviceStatement '}' { TwoPortDevice $1 $5 }


SimpleDeviceType :: { SimpleDeviceType }
SimpleDeviceType : 'Resistor' { Resistor } 
  | 'Inductor' { Inductor }
  | 'Capacitor' { Capacitor }


DeviceStatement :: { DeviceStatement }
DeviceStatement : LHSExpression '=' RHSExpression { DeviceStatement $1 $3 } 


ListDeviceStatement :: { [DeviceStatement] }
ListDeviceStatement : DeviceStatement ';' { (:[]) $1 } 
  | DeviceStatement ';' ListDeviceStatement { (:) $1 $3 }


LHSExpression :: { LHSExpression }
LHSExpression : 'value' { LHSExpression_value } 
  | 'input' { LHSExpression_input }
  | 'output' { LHSExpression_output }
  | Ident { LHSExpressionIdent $1 }


RHSExpression :: { RHSExpression }
RHSExpression : SimpleExpression { RHSExpressionSimpleExpression $1 } 
  | Expression { RHSExpressionExpression $1 }


SimpleExpression :: { SimpleExpression }
SimpleExpression : Double { ExpressionDouble $1 } 
  | Integer { ExpressionInteger $1 }


Expression :: { Expression }
Expression : PortExpression { PortExpr $1 } 
  | MathExpression { MathExpr $1 }


PortExpression :: { PortExpression }
PortExpression : '(' PortName ',' PortName ')' { PortExpression $2 $4 } 


MathExpression :: { MathExpression }
MathExpression : Expression '*' Expression { MathExpression_1 $1 $3 } 
  | Expression '+' Expression { MathExpression_2 $1 $3 }
  | Expression '/' Expression { MathExpression_3 $1 $3 }
  | Expression '-' Expression { MathExpression_4 $1 $3 }
  | '(' MathExpression ')' { MathExpression_5 $2 }
  | Ident { MathExpressionIdent $1 }


InstanceName :: { InstanceName }
InstanceName : Ident { InstanceName $1 } 


PortName :: { PortName }
PortName : Ident { PortNameIdent $1 } 
  | Integer { PortNameInteger $1 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

