module Network.Devices where 

import Data.Either

-- Node is a point in fabric. 
data Node = Node {node :: (Int, Int)} deriving (Show, Eq) 

-- Wire is a list of Nodes which are connected. 
data Wire = Wire { wire :: [Node] } deriving (Show, Eq)

-- Fabric has devices and wires. It can be used to draw the circuit.
data Fabric = Fabric {network :: [Device], topology :: [Wire] } deriving Show 

-- This is port 
data Port = Port {
    portID :: String
    , position :: Node
    , voltage :: Float 
    , current :: Float 
    } deriving (Show, Eq)

-- Device 
data DeviceType = Res 
    | Cap 
    | Ind 
    | Mos 
    | Fet 
    | Dio 
    | Zen
    | Unknown 
    deriving (Eq, Show)

-- Device
data Device = Device {
    did :: Int
    , dname :: String
    , dtype :: DeviceType
    , location :: Node
    , ports :: ([Port], [Port])
    , value :: Float
    , parameters :: [String]
    }  deriving (Show, Eq)

-- Default constructor 
defaultDevice = Device {
     dtype = Unknown
    , did = 0
    , dname = ""
    , location = Node { node = (0, 0) }
    , parameters = []
    , value = 0.0
    , ports = ([], [])
    } 

-- Device Statements 
data StmtType =
    InPortExpr { inPorts :: [String] }
    | OutPortExpr { outPorts :: [String] }
    | ValueExpr { vValue :: Double }
    | ParamExpr { paramName :: String, paramValue :: Double }
    | InitExpr { pName :: String, pValue :: Double }
    | FunctionExpr { }
    | UnknownExpr
    deriving(Eq, Show)

defaultStmtType :: StmtType 
defaultStmtType = UnknownExpr

-- Port type 
data PortType = InPort | OutPort deriving (Show, Eq)

-- Type of device statement
data DeviceStmt = DeviceStmt {
    expr :: String 
    , stmt :: StmtType 
    , comment :: String 
    , spiceStmt :: String
    , texStmt :: String 
    , tikzStmt :: String 
    } deriving (Show, Eq)

defaultStmt = DeviceStmt {
    expr = ""
    , stmt = defaultStmtType 
    , comment = ""
    , spiceStmt = ""
    , texStmt = ""
    , tikzStmt = ""
    }
