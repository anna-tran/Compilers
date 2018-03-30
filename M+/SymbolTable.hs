module SymbolTable (ST,empty,newScope,insert,lookUp,stReturn,delete)
where 
-- instance (Out M_prog) => Out (M_prog)
import SymbolTypes
import SkelM
import AstM
import Control.Monad.State
import Data.Maybe

-- TODO
stReturn :: ST -> M_type
stReturn [] = error "Symbol table is empty!"
stReturn (Symbol_table(L_FUN mt,_,_,_):rest) = mt
stReturn st = error "Current symbol table is not of a function!"

-- The empty symbol table and adding a level
empty :: ST 
empty = []

newScope :: ScopeType -> ST -> ST 
newScope scope st = (Symbol_table(scope,0,0,[])):st

-- "found" converts the data retrieved into the form required by the lookup from the internal form
-- "find_level" searches a level for the symbol
-- "find" searches the levels in turn until the symbol is found raising an exception when it is not found

lookUp :: ST -> String -> Maybe SYM_I_DESC 
lookUp s x = find 0 s 
    where
        found level (Var_attr(offset,mtype,dim)) 
                    =  Just (I_VARIABLE(level,offset,mtype,dim))
        found level (Fun_attr(label,argTypes,mtype)) 
                    = Just (I_FUNCTION(level,label,argTypes,mtype))

        find_level ((str,v):rest)
            | x == str = Just v
            | otherwise =  find_level rest
        find_level [] = Nothing

        find n [] = Nothing
        find n (Symbol_table(_,_,_,v):rest) = 
             (case find_level v of 
              Just v -> found n v
              Nothing -> find (n+1) rest)



-- function label num, symtable, symdesc, new_label_num and symtable
insert :: Int -> ST -> SYM_DESC -> (Int,ST)
insert n [] d =  error "Symbol table error: insertion before defining scope."
insert n ((Symbol_table(sc,nL,nA,sL)):rest) (ARGUMENT(str,t,dim)) 
       | (in_index_list str sL) = error ("Symbol table error: " ++ str ++" is already defined.")
       | otherwise = (n,Symbol_table(sc,nL,nA+1
                         ,(str,Var_attr(negate (nA+4),t,dim)):sL):rest)

insert n ((Symbol_table(sc,nL,nA,sL)):rest) (VARIABLE (str,t,dim)) 
       | (in_index_list str sL) = error ("Symbol table error: "++ str ++" is already defined.")
       | otherwise = (n,Symbol_table(sc,nL+1,nA
                         ,(str,Var_attr(nL+1,t,dim)):sL):rest)
      
insert n ((Symbol_table(sc,nL,nA,sL)):rest) (FUNCTION (str,ts,t))
       | in_index_list str sL = error ("Symbol table error: "++str++" is already defined.")
       | otherwise = (n+1,(Symbol_table(sc,nL,nA,(str,Fun_attr(getlabel n "fn",ts,t)):sL)):rest)    

getlabel :: Int -> String -> String
getlabel n str = str ++ (show n)

in_index_list :: String -> [(String, SYM_VALUE)] -> Bool
in_index_list str [] = False
in_index_list str ((x,_):xs)
        | str == x = True
        | otherwise = in_index_list str xs

delete :: ST -> ST
delete [] = []
delete (st:sts) = sts















