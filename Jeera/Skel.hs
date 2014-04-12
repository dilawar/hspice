module Jeera.Skel where

-- Haskell module generated by the BNF converter

import Jeera.Abs
import Jeera.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transProgram :: Program -> Result
transProgram x = case x of
  Program statements  -> failure x


transStatement :: Statement -> Result
transStatement x = case x of
  DeviceDecl devicedecl  -> failure x


transDeviceDecl :: DeviceDecl -> Result
transDeviceDecl x = case x of
  SimpleDevice instancename simpledevicetype devicestatements  -> failure x


transSimpleDeviceType :: SimpleDeviceType -> Result
transSimpleDeviceType x = case x of
  Resistor  -> failure x
  Inductor  -> failure x


transDeviceStatement :: DeviceStatement -> Result
transDeviceStatement x = case x of
  DeviceStatement lhsexpression rhsexpression  -> failure x


transLHSExpression :: LHSExpression -> Result
transLHSExpression x = case x of
  LHSExpression id  -> failure x


transRHSExpression :: RHSExpression -> Result
transRHSExpression x = case x of
  RHSExpressionInteger n  -> failure x
  RHSExpressionDouble d  -> failure x


transInstanceName :: InstanceName -> Result
transInstanceName x = case x of
  InstanceName id  -> failure x



