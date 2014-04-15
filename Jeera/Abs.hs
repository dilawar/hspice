module Jeera.Abs where

-- Haskell module generated by the BNF converter


newtype Ident = Ident String deriving (Eq,Ord,Show)
data Program =
   Program [Statement]
  deriving (Eq,Ord,Show)

data Statement =
   DeviceDecl DeviceDecl
  deriving (Eq,Ord,Show)

data DeviceDecl =
   Device InstanceName DeviceType [DeviceStatement]
  deriving (Eq,Ord,Show)

data DeviceType =
   Resistor
 | Inductor
 | Capacitor
 | VSource
 | ISource
 | GenericDevice
  deriving (Eq,Ord,Show)

data DeviceStatement =
   PortDeclaration PortDirection [PortName]
 | ValueExpr RHSDeviceExpr
 | ParameterAssignmentExpr ParameterName RHSDeviceExpr
 | InitialConditionExpr ParameterName RHSDeviceExpr
 | PortRelation FunctionOnPort PortName RHSDeviceExpr
  deriving (Eq,Ord,Show)

data PortRelationExpr =
   PortRelationExpr FunctionOnPort PortName
  deriving (Eq,Ord,Show)

data PortDirection =
   InputPort
 | OutputPort
  deriving (Eq,Ord,Show)

data FunctionOnPort =
   FunctionOnPort_V
 | FunctionOnPort_I
  deriving (Eq,Ord,Show)

data ParameterName =
   ParameterName Ident
  deriving (Eq,Ord,Show)

data RHSDeviceExpr =
   RHSDeviceExprSimpleExpr SimpleExpr
 | RHSDeviceExprPortRelationExpr PortRelationExpr
 | RHSDeviceExprMathExpr MathExpr
 | RHSDeviceExprExpr Expr
  deriving (Eq,Ord,Show)

data SimpleExpr =
   ExprDouble Double
 | ExprInteger Integer
  deriving (Eq,Ord,Show)

data Expr =
   MathExpr MathExpr
 | NumericExpr NumericExpr
  deriving (Eq,Ord,Show)

data NumericExpr =
   NumericExprInteger Integer
 | NumericExprDouble Double
  deriving (Eq,Ord,Show)

data PortExpr =
   PortExpr PortName PortName
  deriving (Eq,Ord,Show)

data MathExpr =
   MathExpr_1 RHSDeviceExpr RHSDeviceExpr
 | MathExpr_2 RHSDeviceExpr RHSDeviceExpr
 | MathExpr_3 RHSDeviceExpr RHSDeviceExpr
 | MathExpr_4 RHSDeviceExpr RHSDeviceExpr
 | MathExpr_5 MathExpr
 | MathExprIdent Ident
  deriving (Eq,Ord,Show)

data InstanceName =
   InstanceName Ident
  deriving (Eq,Ord,Show)

data PortName =
   PortNameIdent Ident
 | PortNameInteger Integer
  deriving (Eq,Ord,Show)

