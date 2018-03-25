module SymbolTable (ST,empty,new_scope,insert,lookup,return)
where 
    import SymbolTypes
    import SkelM
    import AST
    empty:: ST 
    new_scope:: ScopeType -> ST -> ST
    insert:: Int -> ST -> SYM_DESC -> (Int,ST)
    lookup:: ST -> String -> SYM_I_DESC
    return:: ST -> M_type

-- The empty symbol table and adding a level
empty = []

new_scope :: ScopeType -> ST -> ST 
new_scope scope s = (Symbol_table(scope,0,0,[])):s

-- "found" converts the data retrieved into the form required by the lookup from the internal form
-- "find_level" searches a level for the symbol
-- "find" searches the levels in turn until the symbol is found raising an exception when it is not found

lookup :: ST -> String -> Maybe SYM_I_DESC = 
lookup s x = find 0 s where
    found level (Var_attr(offset,mtype,dim)) 
                =  I_VARIABLE(level,offset,mtype,dim)
    found level (Fun_attr(label,arg_Type,mtype)) 
                = I_FUNCTION(level,label,arg_Type,mtype)
--    found level (Con_attr(offset,arg_types,user_type))
--                = I_CONSTRUCTOR (offset,arg_types,user_type)
--    found level (Typ_attr user_types)
--                = I_TYPE user_types

    find_level ((str,v):rest)
        | x == str = Just v
        | otherwise =  find_level rest
    find_level [] = Nothing

    find n [] = error ("Could not find "++ str)
    find n (Symbol_table(_,_,_,v):rest) = 
         (case find_level v of 
          Just v -> found n v
          Nothing -> find (n+1) rest)

-- function label num, symtable, symdesc, new_label_num and symtable
insert :: Int -> ST -> SYM_DESC -> (Int,ST)
insert n [] d =  error "Symbol table error: insertion before defining scope."
insert n ((Symbol_table(sc,nL,nA,sL)):rest) (ARGUMENT(str,t,dim)) 
       | (in_index_list str sL) = error ("Symbol table error: " ++ str ++"is already defined.")
       | otherwise = (n,Symbol_table(sc,nL,nA+1
                         ,(str,Var_attr(~(nA+4),T,dim))::sL))
insert n ((Symbol_table(sc,nL,nA,sL)):rest) (VARIABLE (str,T,dim)) 
       | (in_index_list str sL) = error ("Symbol table error: "++ str ++"is already defined.")
       | otherwise = (n,Symbol_table(sc,nL+1,nA
                         ,(str,Var_attr(nL+1,T,dim))::sL))
insert n ((Symbol_table(sc,nL,nA,sL)):rest) FUNCTION (str,Ts,T)
       | in_index_list str sL = error ("Symbol table error: "++str++"is already defined.")
       | otherwise = (n+1,(Symbol_table(sc,nL,nA,(str,Fun_attr(getlabel n "fn",Ts,T)):sL)
                          ):rest)
-- omitting datatype and constructor for M+
        where 
            in_index_list str [] = False
            in_index_list str ((x,_):xs)
                    | str==x = True
                    | otherwise = in_index_list str xs
-- DONE
insertMult :: Int -> ST -> [SYM_DESC] -> (Int,ST)
insertMult n st [] = (n,st)
insertMult n st (x:xs) = insertSymDescs n1 st1 xs
    where
        (n1, st1) = insert n st x

addProgSymtable :: M_prog -> ST
addProgSymtable M_prog (decls, stmts) = st3
    where
        et = empty
        st = new_scope L_PROG empty
        (lNum1, st1) = insertMult 0 st (map symDescDecl decls)
        (lNum2, st2) = addFuncSymtables (filter (== (M_fun x)) decls) lNum1 st1
        (lNum3, st3) = addBlockSymtables (filter (== (M_block x)) stmts) lNum2 st2
-- DONE?
addFuncSymtables :: [M_decl] -> Int -> ST -> (Int,ST)
addFuncSymtables [] lNum st -> (lNum,st)
addFuncSymtables (x:xs) lNum st = (addFuncSymtables xs lNum1 st1)    -- keep on same level
    where
        (lNum1,st1) = addFuncTable x lNum st

-- DONE?
addFuncTable :: M_decl -> Int -> ST -> (Int,ST)
addFuncTable (M_var x) lNum st = (lNum st)
addFuncTable (M_fun (id,args,retType,decls,stmts)) lNum st = (lNum3, st3)
    where
        st0 = new_scope (L_FUN retType) st
        (lNum1, st1) = insertMult lNum st0 (map symDescDecl decls)
        (lNum2, st2) = insertMult lNum1 st1 (map symDescFuncArgs args)
        (lNum3, st3) = addFuncSymtables (filter (== (M_fun x)) decls) lNum2 st2


addBlockSymtables :: [M_stmt] -> Int -> ST -> (Int,ST)
addBlockSymtables [] lNum st -> (lNum,st)
addBlockSymtables (x:xs) lNum st = addBlockSymtables xs lNum st
    where
        (lNum1, st1) = addBlockTable x lNum st


addBlockTable :: M_stmt -> Int -> ST -> (Int,ST)
addBlockTable (M_block (decls,stmts) lNum st = (lNum3,st3)
    where
        st0 = new_scope L_BLK st
        (lNum1, st1) = insertMult lNum st0 (map symDescDecl decls)
        (lNum2, st2) = addFuncSymtables (filter (== (M_fun x)) decls) lNum1 st1
        (lNum3, st3) = addBlockSymtables (filter (== (M_block x)) stmts) lNum2 st2

addBlockTable x lNum st = (lNum,st)
         
symDescFuncArg :: (String,Int,M_type) -> SYM_DESC
symDescFuncArg (id,dim,mtype) = ARGUMENT (id,mtype,dim) 

-- DONE        
symDescDecl :: M_decl -> SYM_DESC
symDescDecl M_var (id, dim, mtype) = VARIABLE (id, mtype, dimLen)
    where
        dimLen = len dim

symDescDecl M_fun (id, args, returntype, decls, stmts) = FUNCTION (id, argList, returntype)
    where
        argList = getFunArgs args

-- DONE
getFunArgs :: [(String,Int,M_type)] -> [(M_type,Int)]
getFunArgs [] = []
getFunArgs ((id,nDim,mtype):xs) = (mtype,nDim) : (getFunArgs xs)
   
















