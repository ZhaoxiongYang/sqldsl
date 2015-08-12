{-# LANGUAGE DeriveFunctor #-} module DSL where
import Data.Map.Strict
import Control.Monad.Free
import Control.Monad

data SqlDslF      next  = DeclareTable (Declaration ()) next
                        | EndProgram
                        deriving (Show, Functor)

data DeclarationF next  = Name        String                 next
                        | PrimaryKey  String  DataType       next
                        | Property    String  DataType       next
                        | Connection  String  ConnectionType next
                        | EndTable
                        deriving (Show, Functor)

data DataType = SqlInt | SqlString | SqlText | SqlNull deriving (Show, Eq)
data ConnectionType = One | Many    deriving (Show, Eq)

type SqlDsl       a = Free SqlDslF a
type Declaration  a = Free DeclarationF a

declareTable :: Declaration () -> SqlDsl ()
declareTable decl = liftF $ DeclareTable decl ()

endProgram :: SqlDsl ()
endProgram = liftF EndProgram

name :: String -> Declaration ()
name str = liftF $ Name str ()

primaryKey :: String -> DataType -> Declaration ()
primaryKey str datatype = liftF $ PrimaryKey str datatype ()

property :: String -> DataType -> Declaration ()
property str datatype = liftF $ Property str datatype ()

connection :: String -> ConnectionType -> Declaration ()
connection str connectiontype = liftF $ Connection str connectiontype ()

endTable :: Declaration ()
endTable = liftF EndTable

properties :: [(String, DataType)] -> Declaration ()
properties []     = return ()
properties (p:ps) = do
  uncurry property p
  properties ps

connections :: [(String, ConnectionType)] -> Declaration ()
connections []     = return ()
connections (c:cs) = do
  uncurry connection c
  connections cs

tablemap = toTableMap table

table = do
  declareTable user
  declareTable essay
  endProgram

user = do
  name "user"
  primaryKey "userid" SqlInt
  properties userProperties
  connections userConnections
  endTable
userProperties =
  [("username", SqlString)
  ,("pwdhash", SqlString)]
userConnections =
  [("essay", Many)
  ,("author", Many)
  ,("essay", Many)
  ,("reading", Many)
  ,("user", Many)]

essay = do
  name "essay"
  primaryKey "essayid" SqlInt
  property "content" SqlText
  connection "user" One
  connection "document" Many
  endTable

document = do
  name "document"
  primaryKey "documentid" SqlInt
  property "content" SqlText
  endTable

-- First, go through, make a map of all connections
-- Then validate that connections really exist
-- Then make a new map with connection types replaced by "many to one", "one to many", "many to many"
-- Then make a data structure that represents the real sql declarations
-- Then make the sqlize program
data Table = Table { tName         :: String
                   , tPrimaryKey   :: (String, DataType)
                   , tProperties   :: [(String, DataType)]
                   , tConnections  :: [(String, ConnectionType)] } deriving (Show)

-- Create full map
toTableMap :: SqlDsl a -> Either String (Map String Table)
toTableMap program = toTableMap' empty program 

toTableMap' :: Map String Table -> SqlDsl a -> Either String (Map String Table)
toTableMap' tableMap (Free EndProgram) = Right tableMap
toTableMap' tableMap (Pure _) = Left "Missing EndProgram statement!"
toTableMap' tableMap (Free (DeclareTable decl next)) = do
  newTable <- toTable decl
  let name = tName newTable
  if member name tableMap
    then Left $ "Table " ++ name ++ " defined twice!"
    else toTableMap' (insert name newTable tableMap) next

toTable :: Declaration a -> Either String Table
toTable decl = toTable' defaultTable decl

defaultTable = Table { tName = ""
                     , tPrimaryKey = ("", SqlNull)
                     , tProperties = []
                     , tConnections = [] }

toTable' :: Table -> Declaration a -> Either String Table
toTable' table statement = case statement of
  Free (Property str dataType next) ->
    toTable' (table { tProperties = (str, dataType) : tProperties table }) next
  Free (Connection str connectionType next) ->
    toTable' (table { tConnections = (str, connectionType) : tConnections table }) next
  Free (Name str next)
    | hasDefaultName table -> toTable' (table { tName = str }) next
    | otherwise            -> Left $ "Table name defined twice. First name: "
                                   ++ tName table ++ ". Second name: " ++ str
  Free (PrimaryKey str dataType next)
    | hasDefaultKey table -> toTable' (table { tPrimaryKey = (str, dataType) }) next
    | otherwise           -> Left $ "Primary key defined twice. First definition: "
                                  ++ show (tPrimaryKey table) ++ ". Second "
                                  ++ "definition: " ++ show (str, dataType)
  Free EndTable
    | fullTable      table  -> Right table
    | hasDefaultName table  -> Left "Table has undefined name!"
    | hasDefaultKey  table  -> Left "Table has undefined primary key!"
    | otherwise             -> Left "Unknown table definition error!"
  Pure next -> Left "Missing EndTable statement!"


--toTable' :: Table -> Declaration a -> Either String Table
--toTable' table (Free (Property str dataType next)) =
--  toTable' (table { tProperties = (str, dataType) : tProperties table }) next
--toTable' table (Free (Connection str connectionType next)) =
--  toTable' (table { tConnections = (str, connectionType) : tConnections table }) next
--toTable' table (Free (Name str next))
--  | hasDefaultName table = toTable' (table { tName = str }) next
--  | otherwise            = Left $ "Table name defined twice. First name: "
--                                ++ tName table ++ ". Second name: " ++ str
--toTable' table (Free (PrimaryKey str dataType next))
--  | hasDefaultKey table = toTable' (table { tPrimaryKey = (str, dataType) }) next
--  | otherwise           = Left $ "Primary key defined twice. First definition: "
--                               ++ show tPrimaryKey table ++ ". Second "
--                               ++ "definition: " ++ show (str, dataType)
--toTable' table (Free EndTable)
--  | fullTable      table  = Right table
--  | hasDefaultName table  = Left "Table has undefined name!"
--  | hasDefaultKey  table  = Left "Table has undefined primary key!"
--  | otherwise             = Left "Unknown table definition error!"
--toTable' table (Pure next) = Left "Missing EndTable statement!"
--
fullTable :: Table -> Bool
fullTable table = not (hasDefaultName table) && not (hasDefaultKey table)

hasDefaultName :: Table -> Bool
hasDefaultName table = tName table == tName defaultTable

hasDefaultKey :: Table -> Bool
hasDefaultKey table = tPrimaryKey table == tPrimaryKey defaultTable

--toTable' :: Table -> Declaration a -> Either String Table
--toTable' table (Name str next)
--  | hasDefaultName table = toTable' (table { tName = str }) next
--  | otherwise            = Left "Table name defined twice. First name: "
--                                ++ tName table ++ ". Second name: " ++ str
--toTable' table (PrimaryKey str dataType next)
--  | hasDefaultKey table = toTable' (table { tPrimaryKey = (str, dataType) }) next
--  | otherwise           = Left "Primary key defined twice. First definition: "
--                               ++ show tPrimaryKey table ++ ". Second "
--                               ++ "definition: " ++ show (str, dataType)
--toTable' table (Property str dataType next) =
--  toTable' (table { tProperties = (str, dataType) : tProperties table }) next
--toTable' table (Connection str connectionType next) =
--  toTable' (table { tConnections = (str, connectionType) : tConnections table }) next
--
