

















































    -- module Wff 
    
import SymbolTypes
import SymbolTable
import AstM
import WffHelper

allwff_decl :: Int -> ST -> [M_decl] -> (Bool,Int,ST)
allwff_decl lNum st [] = (True,lNum,st)
allwff_decl lNum st (d:ds) = (b,lNum2,st2)
    where
        (b1,lNum1,st1) = wff_decl lNum st d 
        (b2,lNum2,st2) = allwff_decl lNum1 st1 ds
        b = b1 && b2


allwff_expr :: ST -> [M_expr] -> Bool
allwff_expr st ss = foldr (\x acc -> let (b,_) = wff_expr st x in b && acc) True ss

allwff_stmt :: Int -> ST -> [M_stmt] -> (Bool,Int)
allwff_stmt lNum st [] = (True,lNum)
allwff_stmt lNum st (s:ss) = (b,lNum2)
    where
        (b1,lNum1) = wff_stmt lNum st s
        (b2,lNum2) = allwff_stmt lNum1 st ss
        b = b1 && b2
    
    
allwff_func_args :: Int -> ST -> [SYM_DESC] -> (Int,ST)
allwff_func_args lNum st [] = (lNum,st)
allwff_func_args lNum st (arg:args) = (lNum2,st2)
    where
        (lNum1,st1) = insert lNum st arg
        (lNum2,st2) = allwff_func_args lNum1 st1 args

allMIntExpr :: ST -> [M_expr] -> Bool
allMIntExpr st es = foldr (\x acc -> let (b,mt) = wff_expr st x in (isMInt mt) && acc) True es        

wff_prog :: M_prog -> Bool 
wff_prog (M_prog (ds,ss)) = b 
    where 
        et = empty
        st = newScope L_PROG et 
        (b1,lNum',st') = allwff_decl 0 st ds 
        (b2,lNum'') = allwff_stmt lNum' st' ss
        b = b1 && b2

-- 1 make sure it has not been declared in current scope
-- 2 make sure that all dims are expr of type int
wff_decl :: Int -> ST -> M_decl -> (Bool,Int,ST)
wff_decl lNum st mv@(M_var (id,dims,mt)) =
    case lookUp st id of
        Nothing -> (b,lNum',st')
            where 
                (b,lNum',st') = insertIfNotLevel id lNum st mv 1
        Just x  -> case x of
            -- same in both cases
            I_VARIABLE(level,_,_,_) -> (b,lNum',st')
                where
                    (b,lNum',st') = insertIfNotLevel id lNum st mv level
            I_FUNCTION(level,_,_,_) -> (b,lNum',st')
                where
                    (b,lNum',st') = insertIfNotLevel id lNum st mv level


--  make sure it has not been declared in current scope
-- 1 create new scope for function
-- 2 insert arguments
-- 3 wff_decl for all func decls
-- 4 wff_stmt for all func stmts                    
-- 5 delete the scope
wff_decl lNum st mf@(M_fun (id,args,retType,decls,stmts)) = 
    case lookUp st id of
        Nothing -> (b,lNum',st')
            where
                (b,lNum',st') = insertIfNotLevel id lNum st mf 1
        Just x -> case x of
            -- same in both cases
            I_VARIABLE(level,_,_,_) -> (b,lNum',st')
                where
                    (b,lNum',st') = insertIfNotLevel id lNum st mf level
            I_FUNCTION(level,_,_,_) -> (b,lNum',st')
                where
                    (b,lNum',st') = insertIfNotLevel id lNum st mf level


insertIfNotLevel :: String -> Int -> ST -> M_decl -> Int -> (Bool,Int,ST)
insertIfNotLevel s lNum st _ 0 = error $ "M_decl: There is an existant declaration for " ++ show s --(False,lNum,st)
insertIfNotLevel _ lNum st (M_var (id,dims,mt)) level = (b',lNum',st') 
    where 
        b' = allMIntExpr st dims
        (lNum',st') = insert lNum st (VARIABLE (id,mt,length dims))
-- Incomplete
insertIfNotLevel _ lNum st (M_fun (id,args,retType,decls,stmts)) level = (b,lNum4,st4)
    where
        -- create new temp scope
        st0 = newScope (L_FUN retType) st
        -- inserts args
        insArgs = map crArgument args
        (lNum1,st1) = allwff_func_args lNum st0 insArgs
        -- inserts decls
        (b2,lNum2,st2) = allwff_decl lNum1 st1 decls
        (b3,lNum3) = allwff_stmt lNum2 st2 stmts 
        -- delete temp scope
        st3 = delete st2
        -- insert the FUNCTION into the current scope
        iFuncArgs = map crIFuncArg args
        (lNum4,st4) = insert lNum3 st3 (FUNCTION (id,iFuncArgs,retType))
        b = b2 && b3












wff_stmt :: Int -> ST -> M_stmt -> (Bool,Int)
wff_stmt lNum st (M_ass (id,dims,e)) = 
    case lookUp st id of
        Nothing -> error $ "M_ass: No existant declaration for " ++ show id --(False,lNum)
        Just x  -> case x of
            I_VARIABLE y    -> (b,lNum)
                where
                    mt1 = getLookupType x
                    b1 = withinDims x dims
                    (b2,mt2) = wff_expr st e
                    (b3,mt3) = sameMtype mt1 mt2
                    b = b1 && b2 && b3
            otherwise       -> error $ "M_ass: Cannot assign to a function " ++ show id --(False,lNum)

wff_stmt lNum st (M_while (e,s)) = (b,lNum)
    where
        (b1, mt) = wff_expr st e
        b2 = isMBool mt
        (b3,_) = wff_stmt lNum st s
        b = b1 && b2 && b3

wff_stmt lNum st (M_cond (e,s1,s2)) = (b,lNum)
    where
        (b1,mt) = wff_expr st e 
        (b2,lNum1) = wff_stmt lNum st s1 
        (b3,lNum2) = wff_stmt lNum1 st s2 
        b4 = isMBool mt 
        b = (b1 && b2 && b3 && b4)

wff_stmt lNum st (M_read (id,dims)) =
    case lookUp st id of
        Nothing -> error $ "M_read: No existant declaration for " ++ show id --(False,lNum)
        Just x  -> case x of
            I_VARIABLE y    -> (b,lNum)
                where
                    b1 = withinDims x dims 
                    b2 = allMIntExpr st dims 
                    b = (b1 && b2)
            otherwise       -> error $ "M_read: Cannot read into a function " ++ show id --(False,lNum)

wff_stmt lNum st (M_print e) = (b,lNum)
    where
        (b,_) = wff_expr st e 

wff_stmt lNum st (M_return e) = (b,lNum)
    where
        (b1,mt1) = wff_expr st e
        mt2 = stReturn st
        (b2,_) = sameMtype mt1 mt2 
        b = (b1 && b2)

-- NOT DONE???
wff_stmt lNum st (M_block (ds,ss)) = (b,lNum2)
    where
        st0 = newScope L_BLK st
        -- wff_decl automatically inserts decls
        (b1,lNum1,st1) = allwff_decl lNum st0 ds 
        (b2,lNum2) = allwff_stmt lNum1 st1 ss 
        -- st3 = delete st2
        b = (b1 && b2)






-- GOOD 
wff_expr :: ST -> M_expr -> (Bool,M_type)
wff_expr st (M_ival n) = (True, M_int)
wff_expr st (M_rval n) = (True, M_real)
wff_expr st (M_bval b) = (True, M_bool)
wff_expr st (M_size (s,n)) = 
    case lookUp st s of
        Nothing     -> error $ "M_size: No existant declaration for " ++ show s --(False, M_int)
        Just x      -> case x of
            I_VARIABLE y    -> (checkDim x n, M_int)
            otherwise       -> error $ "M_size: Cannot get size of a variable " ++ show s --(False, M_int)
wff_expr st (M_id (s,es)) = 
    case lookUp st s of
        Nothing -> error $ "M_id: No existant declaration for " ++ show s --(False, M_int)
        Just x  -> case x of
            (I_VARIABLE y)    -> (b,mt)
                where
                    mt = getLookupType x
                    b1 = allMIntExpr st es
                    b2 = withinDims x es
                    b = b1 && b2
            otherwise       -> error $ "M_id: Cannot perform ID on a function " ++ show s--(False,M_int)

wff_expr st (M_app (op,es)) = (b,mt)
    where
        
        (b1,mts) = wff_op st op
        (b2,mt)  = sameTypewff_expr st es
        b = (b1 && b2)



sameTypewff_expr :: ST -> [M_expr] -> (Bool,M_type)
sameTypewff_expr st [] = (False,M_int)
sameTypewff_expr st [e] = (b,mt)
    where
        (b,mt) = wff_expr st e
sameTypewff_expr st (e:es) = (b,mt1)
    where
        (b1,mt1) = wff_expr st e 
        (b2,mt2) = sameTypewff_expr st es 
        (b3,mt3) = sameMtype mt1 mt2
        b = (b1 && b2 && b3)





-- GOOD
wff_op :: ST -> M_operation -> (Bool,[M_type])
wff_op st (M_fn s) = 
    case lookUp st s of
        Nothing -> error $ "M_op: No existant declaration for function " ++ show s -- (False,[])
        Just x  -> case x of 
            I_FUNCTION(level,label,args,retType)    ->  (True,[retType])
            otherwise                               ->  error $ "M_op Cannot call a variable " ++ show s--(False,[])    
wff_op st op
    | op `elem` [M_add , M_mul , M_sub , M_div , M_neg]                     =  (True,[M_int,M_real])
    | op `elem` [M_lt , M_le , M_gt , M_ge , M_eq , M_not , M_and , M_or]   =  (True,[M_bool])
    | op `elem` [M_float , M_floor , M_ceil]                                =  (True,[M_bool])
    | otherwise                                                             = error $ "Operation " ++ (show op) ++ " does not exist"-- (False,[])