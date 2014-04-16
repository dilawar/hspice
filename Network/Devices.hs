module Network.Devices where 

import qualified Data.Map as M

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
    , voltage :: Double 
    , current :: Double 
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
    , value :: Double
    , initVal :: Double
    , parameters :: M.Map String Double
    }  deriving (Show, Eq)

-- Default constructor 
defaultDevice = Device {
     dtype = Unknown
    , did = 0
    , dname = ""
    , location = Node { node = (0, 0) }
    , parameters = M.empty
    , value = 0.0
    , initVal = 0.0
    , ports = ([], [])
    } 

-- Device Statements 
data StmtType =
    InPortExpr { inPorts :: [String] }
    | OutPortExpr { outPorts :: [String] }
    | ValueExpr { vValue :: Double }
    | ParamExpr { pName :: String, pValue :: Double }
    | InitExpr {  initValue :: Double }
    | UnknownExpr
    deriving(Eq, Show)

-- Atomic expression 
defaultStmtType :: StmtType 
defaultStmtType = UnknownExpr

-- Port type 
data PortType = InPort | OutPort deriving (Show, Eq)

-- Type of device statement
data DeviceStmt = DeviceStmt {
     stmts :: [StmtType]
    } deriving (Show, Eq)

defaultStmt = DeviceStmt {
    stmts = [] 
    }
