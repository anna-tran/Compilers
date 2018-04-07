module Wff where 

import SymbolTypes
import SymbolTable
import AstM
import WffHelper
import Data.Maybe
import IR

-- returns (label num, symtable, # local vars, var array dimensions)
allWffDecl :: Int -> ST -> [M_decl] -> SF (Int,ST,Int,[(Int,[I_expr])])
allWffDecl lNum st [] = SS (lNum,st,0,[])
allWffDecl lNum st (d:ds) = sf
    where
        sf1 = wffDecl lNum st d
        sf2 = if (isSS sf1) 
                then let (lNum1,st1,_,_) = fromSS sf1 in allWffDecl lNum1 st1 ds
                else FF $ ""
        sf = if (isSS sf1) && (isSS sf2)
                then let
                    (_,_,nVar1,iArrDims1) = fromSS sf1
                    (lNum2,st2,nVar2,iArrDims2) = fromSS sf2
                    in SS (lNum2,st2,nVar1+nVar2,iArrDims1 ++ iArrDims2)
                else FF $ fromFF sf1 ++ fromFF sf2
            



allWffStmt :: Int -> ST -> [M_stmt] -> SF (Int,[I_stmt])
allWffStmt lNum st [] = SS (lNum,[])
allWffStmt lNum st (s:ss) = sf
    where
        sf1 = wffStmt lNum st s
        sf2 = if (isSS sf1)
            then let (lNum',_) = fromSS sf1 in allWffStmt lNum' st ss
            else FF ""
        sf = if ((isSS sf1) && (isSS sf2))
            then let
                (_,is) = fromSS sf1
                (lNum',iss) = fromSS sf2
                in SS (lNum',is:iss)
            else FF $ fromFF sf1 ++ fromFF sf2
    
    
allWffFuncArgs :: Int -> ST -> [SYM_DESC] -> (Int,ST)
allWffFuncArgs lNum st [] = (lNum,st)
allWffFuncArgs lNum st (arg:args) = (lNum2,st2)
    where
        (lNum1,st1) = insert lNum st arg
        (lNum2,st2) = allWffFuncArgs lNum1 st1 args

allMIntExpr :: ST -> [M_expr] -> Bool
allMIntExpr st es = foldr (\x acc -> 
    let sf1 = wffExpr st x in
        if (isSS sf1)
        then let 
                (mt,_) = fromSS sf1
                b = isMInt mt
            in b && acc
        else False) True es   
        

-- here we create the list of fbodies
stepIntoAllFunc :: Int -> ST -> [M_decl] -> SF (Int,ST,[I_fbody])
stepIntoAllFunc lNum st [] = SS (lNum,st,[])
stepIntoAllFunc lNum st (d:ds) = sf
    where
        sf1 = stepIntoFunc lNum st d
        sf2 = if (isSS sf1)
                then let 
                        (lNum',st',_) = fromSS sf1
                    in stepIntoAllFunc lNum' st' ds
                else FF ""
        sf = if (isSS sf1) && (isSS sf2)
                then let
                        (lNum1,st1,iFbody) = fromSS sf1 
                        (lNum2,st2,iFbodies) = fromSS sf2
                    in SS (lNum2,st2,iFbody ++ iFbodies)                     
                else FF $ fromFF sf1 ++ fromFF sf2
        
stepIntoFunc :: Int -> ST -> M_decl -> SF (Int,ST,[I_fbody])
stepIntoFunc lNum st (M_var _) = SS (lNum,st,[])
stepIntoFunc lNum st (M_fun (id,args,retType,decls,stmts)) = sf    
    where
        -- create new temp scope
        st0 = newScope (L_FUN retType) st
        --
        -- *** allWffArgs ***
        --
        insArgs = map crArgument args
        (lNum1,st1) = allWffFuncArgs lNum st0 insArgs
        --
        -- *** allWffDecl ***
        --
        sf1 = allWffDecl lNum1 st1 decls
        sf2 = if (isSS sf1)
                then let (lNum',st',_,_) = fromSS sf1 in stepIntoAllFunc lNum' st' decls
                else FF $ ""
        --
        -- *** allWffStmt ***
        --
        sf3 = if (isSS sf2)
                then let (lNum',st',_) = fromSS sf2 in allWffStmt lNum' st' stmts 
                else FF $ ""
        --
        -- *** makeFbody ***
        --                
        sf = if (isSS sf1) && (isSS sf2) && (isSS sf3)
                then let
                    nArg = length argTypes
                    (_,_,nVar,iArrDims) = fromSS sf1
                    (_,st',iFbodies) = fromSS sf2
                    (lNum',iStmts) = fromSS sf3
                    (I_FUNCTION(_,label,argTypes,_)) = fromJust $ lookUp st' id
                    -- delete temp scope
                    st'' = delete st'
                    in SS (lNum',st'',[IFUN (label,iFbodies,nVar,nArg,iArrDims,iStmts) ])
                else FF $ fromFF sf1 ++ fromFF sf2 ++ fromFF sf3
                


                                
wffProg :: M_prog -> SF (I_prog)
wffProg (M_prog (ds,ss)) = sf
    where 
        iprog = crEmptyIProg
        et = empty
        st = newScope L_PROG et 
        --
        -- *** allWffDecl ***
        --
        sf1 = allWffDecl 0 st ds
        sf2 = if (isSS sf1)
            then let (lNum',st',_,_) = fromSS sf1 in stepIntoAllFunc lNum' st' ds
            else FF $ ""
        --
        -- *** allWffStmt ***
        --
        sf3 = if (isSS sf2)
            then let (lNum',st',_) = fromSS sf2 in allWffStmt lNum' st' ss
            else FF $ ""
        --
        -- *** makeIProg ***
        --
        sf = if ((isSS sf1) && (isSS sf2) && (isSS sf3))
            then let
                (_,_,nVar,iArrDims) = fromSS sf1
                (_,st',iFbodies) = fromSS sf2
                (_,iStmts) = fromSS sf3
                in SS (IPROG (iFbodies,nVar,iArrDims,iStmts))
            else FF $ fromFF sf1 ++ fromFF sf2 ++ fromFF sf3



-- returns (label num, symtable, # local vars created (0 or 1), array dimensions only if var is an array)
            
-- 1 make sure it has not been declared in current scope
-- 2 make sure that all dims are expr of type int
wffDecl :: Int -> ST -> M_decl -> SF (Int,ST,Int,[(Int,[I_expr])])
wffDecl lNum st mv@(M_var (id,dims,mt)) =
    --
    -- *** lookUp ***
    --    
    case lookUp st id of      
        Nothing -> sf
            where 
                sf = insertIfNotLevel lNum st mv 1
        Just x  -> case x of
            I_VARIABLE(level,_,_,_) -> sf
                where
                    sf = insertIfNotLevel lNum st mv level
            I_FUNCTION(level,_,_,_) -> sf
                where
                    sf = insertIfNotLevel lNum st mv level


--  make sure it has not been declared in current scope
wffDecl lNum st mf@(M_fun (id,args,retType,decls,stmts)) = 
    --
    -- *** lookUp ***
    --    
    case lookUp st id of
        Nothing -> sf
            where
                sf = insertIfNotLevel lNum st mf 1
        Just x -> case x of
            I_VARIABLE(level,_,_,_) -> sf
                where
                    sf = insertIfNotLevel lNum st mf level
            I_FUNCTION(level,_,_,_) -> sf
                where
                    sf = insertIfNotLevel lNum st mf level


-- returns (label num, symtable, # local vars created (0 or 1), array dimensions only if var is an array)
--         ( Int,       ST,             Int,                        [(Int,[I_expr])]                    )
insertIfNotLevel :: Int -> ST -> M_decl -> Int -> SF (Int,ST,Int,[(Int,[I_expr])])
insertIfNotLevel lNum st (M_var (id,_,_)) 0     = FF $ "ERROR in declaration: There is an existant declaration for " ++ show id ++ "\n"
insertIfNotLevel lNum st (M_fun (id,_,_,_,_))0  = FF $ "ERROR in declaration: There is an existant declaration for " ++ show id ++ "\n"
insertIfNotLevel lNum st (M_var (id,dims,mt)) level = sf
    where 
        --
        -- *** allMIntExpr ***
        --        
        b = allMIntExpr st dims
        sf1 = trIExprs st dims
        --
        -- *** maybeInsert ***
        --        
        sf = if b && (isSS sf1)
                then let
                    -- insert var decl, then look it up to get offset
                    (lNum',st') = insert lNum st (VARIABLE (id,mt,length dims))
                    in case length dims of
                            0         -> SS (lNum',st',1,[])
                            otherwise -> let
                                (I_VARIABLE (_,offset,_,_)) = fromJust $ lookUp st' id
                                arrDims = fromSS sf1
                                in SS (lNum',st',1,[(offset,arrDims)])
                else FF $ "ERROR in declaration: Incompatible array dimensions for " ++ show id ++ "\n"
insertIfNotLevel lNum st (M_fun (id,args,retType,decls,stmts)) level = sf
    where
        --
        -- *** maybeInsert ***
        --        
        iFuncArgs = map crIFuncArg args
        (lNum',st') = insert lNum st (FUNCTION (id,iFuncArgs,retType))
        sf = SS (lNum',st',0,[])













wffStmt :: Int -> ST -> M_stmt -> SF (Int,I_stmt)
wffStmt lNum st (M_ass (id,dims,e)) = 
    --
    -- *** lookUp ***
    --    
    case lookUp st id of
        Nothing -> FF $ "ERROR in assignment: No existant declaration for " ++ show id ++ "\n"
        Just x  -> case x of
            I_VARIABLE (level,offset,mtype,dim) -> sf
                where
                    mt1 = getLookupType x
                    sf1 = wffExpr st e
                    --
                    -- *** allMIntExpr ***
                    --                    
                    b' = allMIntExpr st dims 
                    --
                    -- *** correctDims ***
                    --                    
                    b''   = exactDims x dims
                   
                    sf2 = if (isSS sf1)
                            then trIExprs st dims
                            else FF ""
                    --      
                    -- *** sameMtype ***
                    --
                    mMt = sameMtype mt1 sf1
                    --
                    -- *** makeAssStmt ***
                    --                    
                    sf = getRetSF b' b'' (isSS sf1) (isSS sf2) (isJust mMt) 
                        where getRetSF b1 b2 b3 b4 b5
                                | b1 && b2 && b3 && b4 && b5 = let
                                                            (_,ie) = fromSS sf1
                                                            indices = fromSS sf2
                                                            in SS (lNum, IASS (level,offset,indices,ie))
                                | not b1                = FF $ "ERROR in assignment: Indices must all be of type integer when assigning to " ++ show id ++ "\n"
                                | not b2                = FF $ "ERROR in assignment: Invalid dimensions for " ++ show id ++ "\n"                                                            
                                | not b5                = FF $ "ERROR in assignment: Cannot assign type " ++ show (fst (fromSS sf1)) ++ " to " ++ show id ++ " of type " ++ show mt1 ++ "\n"
                                
                                | otherwise             = FF $ fromFF sf1 ++ fromFF sf2
                    
            otherwise       -> FF $ "ERROR in assignment: Cannot assign to a function " ++ show id ++ "\n" 



wffStmt lNum st (M_while (e,s)) = sf
    where
        sfe = wffExpr st e 
        --
        -- *** isBool ***
        --        
        b = if (isSS sfe)
            then let (mt,_) = fromSS sfe in isMBool mt
            else False
        sfs = wffStmt lNum st s
        --
        -- *** makeWhileStmt ***
        --        
        sf = if (b && (isSS sfe) && (isSS sfs))
            then let
                (_,ie) = fromSS sfe
                (lNum',is) = fromSS sfs
                in SS (lNum',IWHILE (ie,is))
            else FF$ fromFF sfe ++ fromFF sfs



wffStmt lNum st (M_cond (e,s1,s2)) = sf
    where
        sfe1 = wffExpr st e 
        --
        -- *** isBool ***
        --        
        b = if (isSS sfe1)
            then let (mt,_) = fromSS sfe1 in isMBool mt
            else False
        sfs1 = wffStmt lNum st s1 
        sfs2 = if (isSS sfs1)
            then let (lNum1,_) = fromSS sfs1 in wffStmt lNum1 st s2
            else FF $ ""
        --
        -- *** makeCondStmt ***
        --            
        sf = getRetSF b (isSS sfe1) (isSS sfs1) (isSS sfs2)
            where getRetSF b1 b2 b3 b4
                    | b1 && b2 && b3 && b4  = let
                                                (_,ie) = fromSS sfe1
                                                (_,is1) = fromSS sfs1
                                                (lNum2,is2) = fromSS sfs2
                                                in SS (lNum2,ICOND (ie,is1,is2))
                    | not b1                = FF $ "ERROR in conditonal: The conditional expression must be of type boolean but is of type " ++ show (fst (fromSS sfe1)) ++ "\n"
                    | otherwise             = FF $ fromFF sfe1 ++ fromFF sfs1 ++ fromFF sfs2
                

    

wffStmt lNum st (M_read (id,dims)) =
    --
    -- *** lookUp ***
    --           
    case lookUp st id of
        Nothing -> FF $ "ERROR in read: No existant declaration for " ++ show id ++ "\n"
        Just x  -> case x of
            I_VARIABLE (level,offset,mtype,nDim)    -> sf
                where
                    --
                    -- *** exactDims ***
                    --                           
                    b' = exactDims x dims 
                    --
                    -- *** allMIntExpr ***
                    --           
                    b'' = allMIntExpr st dims 
                    --
                    -- *** makeIRead ***
                    --                               
                    sf1 = trIExprs st dims
                    sf = getRetSF b' b'' (isSS sf1)
                        where getRetSF b1 b2 b3 
                                | b1 && b2 && b3    = let ies = fromSS sf1 in case mtype of
                                                            M_int -> (SS (lNum,IREAD_I (level,offset,ies)))
                                                            M_real -> (SS (lNum,IREAD_F (level,offset,ies)))
                                                            M_bool -> (SS (lNum,IREAD_B (level,offset,ies)))
                                | not b1               = FF $ "ERROR in read: To read into variable, must supply " ++ show nDim ++ " dimensions\n" 
                                | not b2               = FF $ "ERROR in read: All dimensions must be of type integer\n"
                                | otherwise         = FF $ fromFF sf1
            otherwise       -> FF $ "ERROR in read: Cannot read into a function " ++ show id ++ "\n"



wffStmt lNum st (M_print e) = sf
    where
        sf1 = wffExpr st e 
        --
        -- *** makeIPrint ***
        --                   
        sf = if (isSS sf1)
            then let  
                (mtype,ie) = fromSS sf1
                in case mtype of
                    M_int -> (SS (lNum,IPRINT_I ie))
                    M_real -> (SS (lNum,IPRINT_F ie))
                    M_bool -> (SS (lNum,IPRINT_B ie))
            else FF $ fromFF sf1         

wffStmt lNum st (M_return e) = sf
    where
        sf1 = wffExpr st e
        --
        -- *** stReturn ***
        --         
        mt = stReturn st
        --
        -- *** sameMtype ***
        --         
        mMt = sameMtype mt sf1
        --
        -- *** makeIReturn ***
        --         
        sf = getRetSF (isSS sf1) (isJust mMt)
            where getRetSF b1 b2
                    | b1 && b2      = let (mtype,ie) = fromSS sf1 in SS (lNum,IRETURN ie)
                    | not b2        = FF $ "ERROR with return: Expected to return " ++ (show mt) ++ " but returns " ++ show (fst (fromSS sf1)) ++ "\n"
                    | otherwise     = FF $ fromFF sf1
        
                

wffStmt lNum st (M_block (ds,ss)) = sf
    where
        st0 = newScope L_BLK st
        --
        -- *** allWffDecl ***
        --         
        sf1 = allWffDecl lNum st0 ds
        sf2 = if (isSS sf1)
            then let (lNum',st',_,_) = fromSS sf1 in stepIntoAllFunc lNum' st' ds
            else FF ""
        --
        -- *** allWffStmt ***
        --             
        sf3 = if (isSS sf2) && (isSS sf1)
            then let (lNum',st',_,_) = fromSS sf1 in allWffStmt lNum' st' ss 
            else FF ""
        --
        -- *** makeIBlock ***
        --             
        sf = if (isSS sf1) && (isSS sf2) && (isSS sf3)
            then let
                (_,_,nVar,iArrDims) = fromSS sf1
                (_,_,iFbodies) = fromSS sf2
                (lNum',iStmts) = fromSS sf3
                in SS (lNum',(IBLOCK (iFbodies,nVar,iArrDims,iStmts)))
            else FF $ fromFF sf1 ++ fromFF sf2 ++ fromFF sf3


trIExprs :: ST -> [M_expr] -> SF [I_expr]
trIExprs st [] = SS []
trIExprs st (e:es) = sf
    where
        sf1 = wffExpr st e
        sf2 = trIExprs st es
        sf = if (isSS sf1) && (isSS sf2)
                then let
                    (_,ie) = fromSS sf1
                    ies = fromSS sf2
                    in SS (ie:ies)
                else FF $ fromFF sf1 ++ fromFF sf2


wffExpr :: ST -> M_expr -> SF (M_type,I_expr)
--
-- *** makeIInt ***
--   
wffExpr st (M_ival n) = SS (M_int, IINT (fromIntegral n))
--
-- *** makeIReal ***
--   
wffExpr st (M_rval n) = SS (M_real,IREAL n)
--
-- *** makeIBool ***
--   
wffExpr st (M_bval b) = SS (M_bool,IBOOL b)
wffExpr st (M_size (s,n)) = 
    --
    -- *** lookUp ***
    --     
    case lookUp st s of
        Nothing     -> FF $ "ERROR in size: No existant declaration for " ++ show s ++ "\n"
        Just x      -> case x of
            I_VARIABLE (level,offset,mtype,dim)    ->
                                --
                                -- *** checkDim ***
                                -- 
                                if (checkDim x n)
                                --
                                -- *** makeISize ***
                                -- 
                                then SS (M_int, ISIZE (level,offset,n+1)) --3rd arg is the dimension number to look at, +1 from the number of dimensions given
                                else FF $ "ERROR in size: Cannot access dimension " ++ show (n+1) ++" for a variable " ++ show s ++ " with " ++ show dim ++ " dimensions\n"
            otherwise       -> FF $ "ERROR in size: Cannot get size of a variable " ++ show s ++ "\n"
wffExpr st (M_id (s,es)) = 
    --
    -- *** lookUp ***
    --         
    case lookUp st s of
        Nothing -> FF $ "ERROR with identifier: No existant declaration for " ++ s ++ "\n"
        Just x  -> case x of
            I_VARIABLE (level,offset,mtype,dim)    -> sf
                where
                    --
                    -- *** allMIntExpr ***
                    --                     
                    b1 = allMIntExpr st es
                    --
                    -- *** withinDims ***
                    --                         
                    b2 = withinDims x es
                    --
                    -- *** makeIId ***
                    --                         
                    mt = getLookupType x
                    sf1 = trIExprs st es
                    sf = if (b1 && b2 && isSS sf1)
                        then let
                            indices = fromSS sf1
                            in SS (mt,IID (level,offset,indices))
                        else FF $ "ERROR with identifier: Invalid dimensions for " ++ show s ++ "\n"
            otherwise       -> FF $ "ERROR with identifier: Cannot perform ID on a function " ++ show s ++ "\n"

            
wffExpr st (M_app (op,es)) = sf
    where
        sf1 = wffOp st op
        -- make sure all expressions for the operation are of the same type
        -- if it's a function make sure it takes the right number of parameters
        --
        -- *** evalWffExprs ***
        --                 
        sf2 = evalwffExprs st es
        sf = if (isSS sf1) && (isSS sf2)
                then let
                    -- need to check that dimensions of arguments to function are matching
                        expectedTypes   = fromSS sf1
                        exprTypes       = fromSS sf2
                        sfmtype         = lookupReturnType st op exprTypes
                        sfies           = trIExprs st es
                        in 
                            --
                            -- *** isExpectedTypes ***
                            --                                 
                            if (exprTypes `elem` expectedTypes && (isSS sfmtype) && (isSS sfies))
                            then let
                                --
                                -- *** makeIApp ***
                                --                                     
                                mt  = fromSS sfmtype
                                ies = fromSS sfies
                                iop = trIOpn st op (fst (head exprTypes))
                                in SS (mt,IAPP (iop,ies))
                            else FF $ "ERROR with operation: Invalid arguments to " ++ show op ++ "\n\tGot " ++ show exprTypes ++ "\n\tExpected one of " ++ show expectedTypes ++ "\n"

                else FF $ fromFF sf1 ++ fromFF sf2


-- return a list of (m_type,num dimensions for the expr)
evalwffExprs :: ST -> [M_expr] -> SF [(M_type,Int)]
evalwffExprs st [] = SS []
evalwffExprs st (e@(M_id (s,arrDims)):es) = sf
    where
        sf1 = wffExpr st e 
        sf2 = evalwffExprs st es 
        sf = if (isSS sf1) && (isSS sf2)
                then 
                    let
                        (mt,_) = fromSS sf1
                        mts = fromSS sf2
                        (I_VARIABLE(level,offset,mtype,nDim)) = fromJust $ lookUp st s
                        nArrDims = nDim - (length arrDims)
                    in SS ((mt,nArrDims):mts)
                else FF $ fromFF sf1 ++ fromFF sf2    

evalwffExprs st (e:es) = sf
    where
        sf1 = wffExpr st e 
        sf2 = evalwffExprs st es 
        sf = if (isSS sf1) && (isSS sf2)
                then 
                    let
                        (mt,_) = fromSS sf1
                        mts = fromSS sf2
                    in SS ((mt,0):mts)
                else FF $ fromFF sf1 ++ fromFF sf2



wffOp :: ST -> M_operation -> SF [[(M_type,Int)]]
wffOp st (M_fn s) = 
    --
    -- *** lookUp ***
    --         
    case lookUp st s of
        Nothing -> FF $ "ERROR with operation: No existant declaration for function " ++ show s ++ "\n"
        Just x  -> case x of 
            --
            -- *** makeMfn ***
            --                 
            I_FUNCTION(level,label,args,retType)    ->  SS [args]
            otherwise                               ->  FF $ "ERROR with operation: Cannot call a variable " ++ show s ++ "\n"
--
-- *** wffOp ***
--                 
wffOp st op
    | op `elem` [M_lt , M_le , M_gt , M_ge , M_eq , M_add , M_mul , M_sub , M_div , M_neg]                     
                                                                            = SS [[(M_int,0),(M_int,0)],[(M_real,0),(M_real,0)]]
    | op `elem` [M_and , M_or]                                              = SS [[(M_bool,0),(M_bool,0)]]
    | op `elem` [M_not]                                                     = SS [[(M_bool,0)]]
    | op `elem` [M_float]                                                   = SS [[(M_int,0)]]
    | op `elem` [M_floor , M_ceil]                                          = SS [[(M_real,0)]]
    | otherwise                                                             = FF $ "ERROR with operation: " ++ (show op) ++ " does not exist" ++ "\n"


    