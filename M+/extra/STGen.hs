module STGen where


import SymbolTypes
import SymbolTable
import AstM

-- DONE
insertMult :: Int -> ST -> [SYM_DESC] -> (Int,ST)
insertMult n st [] = (n,st)
insertMult n st (x:xs) = insertMult n1 st1 xs
    where
        (n1, st1) = insert n st x

addProgSymtable :: M_prog -> (Int,ST)
addProgSymtable (M_prog (decls, stmts)) = (lNum2,st2)
    where
        et = empty
        st = new_scope L_PROG et
        (lNum1, st1) = insertMult 0 st (map symDescDecl decls)
        (lNum2, st2) = stepInStmts decls stmts lNum1 st1

-- for each stmt, map to the symbol table
-- decls of current scope, stmts of curr scope, label num, symtable
stepInStmts :: [M_decl] -> [M_stmt] -> Int -> ST -> (Int,ST)
stepInStmts decls [] lNum st = (lNum,st)
stepInStmts decls (x:xs) lNum st = stepInStmts decls xs lNum1 st1
    where
        (lNum1,st1) = stepInStmt decls x lNum st


stepInStmt :: [M_decl] -> M_stmt -> Int -> ST -> (Int,ST)
stepInStmt decls (M_ass (id,dims,expr)) lNum st = case (lookUp st id) of
    I_FUNCTION(level,label,args,retType)    -> error $ "Cannot assign value to function " ++ id
    -- still haven't checked if index of array are valid
    I_VARIABLE(level,offset,mtype,dim)      ->  if withinDims dim dims
                                                then (stepInExpr decls expr lNum st)
                                                else error $ "Incompatible dimensions for " ++ id
stepInStmt decls (M_while (expr,stmt)) lNum st = (lNum2,st2)
    where
        (lNum1,st1) = stepInExpr decls expr lNum st
        (lNum2,st2) = stepInStmt decls stmt lNum1 st1
stepInStmt decls (M_cond (expr,s1,s2)) lNum st = (lNum3,st3)
    where
        (lNum1,st1) = stepInExpr decls expr lNum st
        (lNum2,st2) = stepInStmt decls s1 lNum1 st1
        (lNum3,st3) = stepInStmt decls s2 lNum2 st2
stepInStmt decls (M_read (id,exprs)) lNum st = case (lookUp st id) of 
    I_FUNCTION(level,label,args,retType)    -> error $ "Cannot read into a function " ++ id
    I_VARIABLE(level,offset,mtype,dim)      -> stepInExprs decls exprs lNum st

stepInStmt decls (M_print expr) lNum st = stepInExpr decls expr lNum st
stepInStmt decls (M_return expr) lNum st = stepInExpr decls expr lNum st
stepInStmt decls (M_block (ds,ss)) lNum st = (lNum2, tail st2)
    where
        st0 = new_scope L_BLK st
        (lNum1, st1) = insertMult lNum st0 (map symDescDecl ds)
        (lNum2, st2) = stepInStmts (ds ++ decls) ss lNum1 st1


stepInExprs :: [M_decl] -> [M_expr] -> Int -> ST -> (Int,ST)
stepInExprs _ [] lNum st = (lNum, st)
stepInExprs decls (e:es) lNum st = stepInExprs decls es lNum1 st1
    where
        (lNum1, st1) = stepInExpr decls e lNum st

-- end at expr unless we get 'apply function f...'
-- should evaluate each expression and return a certain type
stepInExpr :: [M_decl] -> M_expr -> Int -> ST -> (Int,ST,M_type)
stepInExpr decls (M_app (M_fn f, exprs)) lNum st = case (lookUp st f) of
    I_VARIABLE(level,offset,mtype,dim)      ->  error (f ++ " is a variable, not a function")
    I_FUNCTION(level,label,args,retType)    ->  do -- build symtable for f
            -- step into all of the nested functions first
            -- insert new scope, then all the args, then the decls, then the
            let (lNum1,st0) = stepInExprs decls exprs lNum st
            let st1 = new_scope (L_FUN retType) st0
            let pFunc = findIdInDecls f decls in case pFunc of
                M_var x                     ->  error $ f ++ " is a variable, not a function"
                M_fun (fId,as,retT,ds,ss)   ->  (lNum4, tail st4)
                    where
                        (lNum2,st2) = insertMult lNum1 st1 (map symDescFuncArg as)
                        (lNum3,st3) = insertMult lNum2 st2 (map symDescDecl ds)
                        (lNum4,st4) = stepInStmts (ds ++ decls) ss lNum3 st3
-- INCOMPLETE
stepInExpr _ (M_size (id,arr_size)) lNum st = case (lookUp st id) of
    I_FUNCTION(level,label,args,retType)    ->  error $ "Cannot find size for function " ++ id
    I_VARIABLE(level,offset,mtype,dim)      ->  if smallerThanDims dim arr_size
                                                then (lNum,st)
                                                else error $ "Incompatible dimensions for " ++ id
stepInExpr decls (M_id (id,dims)) lNum st = case lookUp st id of
    I_FUNCTION(level,label,args,retType)    ->  error $ "A function " ++ id ++ " is not an identifier"
    -- still haven't checked if index of array are valid
    I_VARIABLE(level,offset,mtype,dim)      ->  if withinDims dim dims
                                                then (lNum,st)
                                                else error $ "Imcompatible dimensions for " ++ id
stepInExpr _ expr lNum st = (lNum,st)

--          num dims, actual dims
withinDims :: Int -> [M_expr] -> Bool
withinDims n dims = (length dims) == n

smallerThanDims :: Int -> Int -> Bool
smallerThanDims n dims = dims < n && dims >= 0

-- assume M_expr are M_ivals (does not account for M_ival derived from some
-- other reference

-- DEAL WITH THIS LATER
-- var dims, given dims
-- validIndices :: [M_expr] -> [M_expr] -> Bool
-- validIndices [] [] = True
-- validIndices [] (x:xs) = False
-- validIndices ((M_ival x):xs) ((M_ival y):ys)
--     | x > y && y >= 0   = validIndices es xs
--     | otherwise         = False
-- validIndices ((M_ival x):xs) ((M_ival y):ys) = validIndices es xs

-- isExprInt :: M_expr -> Bool
-- isExprInt (M_ival x) = True
-- isExprInt (M_size x) = True
-- isExprInt (M_id)

-- the closer the beginning of the list, the closer to the current scope
findIdInDecls :: String -> [M_decl] -> M_decl
findIdInDecls id [] = error $ "Cannot find " ++ id ++ " in declaration list!"
findIdInDecls id (v@(M_var (vId,_,_)):xs)
    | id == vId     = v
    | otherwise     = findIdInDecls id xs
findIdInDecls id (f@(M_fun(fId,_,_,_,_)):xs)
    | id == fId     = f
    | otherwise     = findIdInDecls id xs


symDescFuncArg :: (String,Int,M_type) -> SYM_DESC
symDescFuncArg (id,dim,mtype) = ARGUMENT (id,mtype,dim) 

-- DONE        
symDescDecl :: M_decl -> SYM_DESC
symDescDecl (M_var (id, dim, mtype)) = VARIABLE (id, mtype, dimLen)
    where
        dimLen = length dim

symDescDecl (M_fun (id, args, returntype, decls, stmts)) = FUNCTION (id, argList, returntype)
    where
        argList = getFunArgs args

-- DONE
getFunArgs :: [(String,Int,M_type)] -> [(M_type,Int)]
getFunArgs [] = []
getFunArgs ((id,nDim,mtype):xs) = (mtype,nDim) : (getFunArgs xs)
 




-- DONE?
addFuncSymtables :: [M_decl] -> Int -> ST -> (Int,ST)
addFuncSymtables [] lNum st = (lNum,st)
addFuncSymtables (x:xs) lNum st = (addFuncSymtables xs lNum1 st1)    -- keep on same level
    where
        (lNum1,st1) = addFuncTable x lNum st

-- DONE?
addFuncTable :: M_decl -> Int -> ST -> (Int,ST)
addFuncTable (M_var _) lNum st = (lNum, st)
addFuncTable (M_fun (id,args,retType,decls,stmts)) lNum st = (lNum3, st3)
    where
        st0 = new_scope (L_FUN retType) st
        (lNum1, st1) = insertMult lNum st0 (map symDescDecl decls)
        (lNum2, st2) = insertMult lNum1 st1 (map symDescFuncArg args)
        (lNum3, st3) = addFuncSymtables (filterFunc decls) lNum2 st2


addBlockSymtables :: [M_stmt] -> Int -> ST -> (Int,ST)
addBlockSymtables [] lNum st = (lNum,st)
addBlockSymtables (x:xs) lNum st = addBlockSymtables xs lNum1 st1
    where
        (lNum1, st1) = addBlockTable x lNum st


addBlockTable :: M_stmt -> Int -> ST -> (Int,ST)
--addBlockTable (M_block (decls,stmts)) lNum st = error "abc"
addBlockTable (M_block (decls,stmts)) lNum st = (lNum3,st3)
    where
        st0 = new_scope L_BLK st
        (lNum1, st1) = insertMult lNum st0 (map symDescDecl decls)
        (lNum2, st2) = addFuncSymtables (filterFunc decls) lNum1 st1
        (lNum3, st3) = addBlockSymtables (filterBlock stmts) lNum2 st2
addBlockTable (M_while (e,s)) lNum st = (lNum1, st1)
    where
        (lNum1, st1) = addBlockTable s lNum st
addBlockTable (M_cond (e,s1,s2)) lNum st = (lNum2, st2)
    where
        (lNum1, st1) = addBlockTable s1 lNum st
        (lNum2, st2) = addBlockTable s2 lNum1 st1
addBlockTable x lNum st = (lNum,st)

filterFunc :: [M_decl] -> [M_decl]
filterFunc decls = [ f | f@(M_fun _) <- decls]

filterBlock :: [M_stmt] -> [M_stmt]
filterBlock stmts = [ b | b@(M_block _) <- stmts]
         
  