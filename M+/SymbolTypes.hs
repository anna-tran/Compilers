module SymbolTypes where
import AstM

data ScopeType = L_PROG | L_FUN M_type | L_BLK | L_CASE
                deriving (Eq, Ord, Read, Show)
-- for insertion
data SYM_DESC = ARGUMENT (String,M_type,Int)
              | VARIABLE (String,M_type,Int)
              | FUNCTION (String,[(M_type,Int)],M_type)
              | DATATYPE String 
              | CONSTRUCTOR (String, [M_type], String)
                deriving (Eq, Ord, Read, Show)
-- for lookup
data SYM_I_DESC = I_VARIABLE (Int,Int,M_type,Int)
                    | I_FUNCTION (Int,String,[(M_type,Int)],M_type)
                    | I_CONSTRUCTOR (Int,[M_type],String)
                    | I_TYPE [String]
                    deriving (Eq, Ord, Read, Show)
-- things that are only used internally
-- var  offset, type, dim
-- fun  id, args, return
data SYM_VALUE = Var_attr (Int,M_type,Int)
              | Fun_attr (String,[(M_type,Int)],M_type)
              | Con_attr (Int, [M_type], String)
              | Typ_attr [String]
                deriving (Eq, Ord, Read, Show)
--                              scope, vars, args, decls
data SYM_TABLE = Symbol_table (ScopeType,Int,Int,[(String,SYM_VALUE)])
                deriving (Eq, Ord, Read)

type ST = [SYM_TABLE]

data SF a = SS a 
          | FF String
          deriving (Eq,Ord,Read,Show)
          
isSS :: SF a -> Bool
isSS (SS x) = True
isSS (FF s) = False

isFF :: SF a -> Bool
isFF (FF s) = True
isFF (SS x) = False

fromSS :: SF a -> a
fromSS (SS x) = x
fromFF :: (Show a) => SF a -> String
fromFF (FF s) = s
fromFF x = ""

showSTList :: ST -> String
showSTList [] = ""
showSTList (x:xs) = (show x) ++ "\n" ++ (showSTList xs)

instance Show SYM_TABLE where
    show (Symbol_table (sc,nV,nA,ds)) =
        "Symtable (" ++ (show sc) ++ "," ++ (show nV) ++ "," ++ (show nA)
        ++ ",\n   [" ++ (listAsStr ds) ++ "]"

listAsStr :: (Show a) => [a] -> String
listAsStr [] = ""
listAsStr [t] = show t
listAsStr (t:ts) = (show t) ++ ",\n    " ++ (listAsStr ts)
