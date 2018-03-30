module Wff where
    
import SymbolTypes
import SymbolTable
import AstM
import WffHelper
import Data.Maybe

allwff_decl :: Int -> ST -> [M_decl] -> SF (Int,ST)
allwff_decl lNum st [] = SS (lNum,st)
allwff_decl lNum st (d:ds) = sf
    where
        sf1 = wff_decl lNum st d
        sf2 = case (isSS sf1) of
            True -> let (lNum1,st1) = fromSS sf1 in allwff_decl lNum1 st1 ds
            False -> FF $ ""
        sf = case ((isSS sf1) && (isSS sf2)) of
            True -> sf2
            False -> FF $ fromFF sf1 ++ fromFF sf2


-- allwff_expr :: ST -> [M_expr] -> SF Bool
-- allwff_expr _ [] = SS True
-- allwff_expr
-- allwff_expr st ss = foldr (\x acc -> let (b,_) = wff_expr st x in b && acc) True ss

allwff_stmt :: Int -> ST -> [M_stmt] -> SF Int
allwff_stmt lNum st [] = SS lNum
allwff_stmt lNum st (s:ss) = sf
    where
        --SF int
        sf1 = wff_stmt lNum st s
        sf2 = if (isSS sf1)
            then let lNum1 = fromSS sf1 in allwff_stmt lNum1 st ss
            else FF $ ""
        sf = if ((isSS sf1) && (isSS sf2))
            then sf2
            else FF $ fromFF sf1 ++ fromFF sf2
    
    
allwff_func_args :: Int -> ST -> [SYM_DESC] -> (Int,ST)
allwff_func_args lNum st [] = (lNum,st)
allwff_func_args lNum st (arg:args) = (lNum2,st2)
    where
        (lNum1,st1) = insert lNum st arg
        (lNum2,st2) = allwff_func_args lNum1 st1 args

allMIntExpr :: ST -> [M_expr] -> Bool
allMIntExpr st es = foldr (\x acc -> 
    let sf1 = wff_expr st x in
        if (isSS sf1)
        then
            let mt = fromSS sf1
                b = isMInt mt
            in b && acc
        else False) True es        

wff_prog :: M_prog -> SF Bool 
wff_prog (M_prog (ds,ss)) = sf
    where 
        et = empty
        st = newScope L_PROG et 
        -- insert all decls prematurely
        sf1 = allwff_decl 0 st ds 
        -- go into each of the fun decls
        sf2 = if (isSS sf1)
            then let (lNum',st') = fromSS sf1 in stepIntoAllFunc lNum' st' ds
            else FF $ ""
        -- insert all of the stmts
        sf3 = if (isSS sf2)
            then let (lNum',st') = fromSS sf2 in allwff_stmt lNum' st' ss
            else FF $ ""
        sf = if ((isSS sf1) && (isSS sf2) && (isSS sf3))
            then SS True
            else FF $ fromFF sf1 ++ fromFF sf2 ++ fromFF sf3

-- 1 make sure it has not been declared in current scope
-- 2 make sure that all dims are expr of type int
wff_decl :: Int -> ST -> M_decl -> SF (Int,ST)
wff_decl lNum st mv@(M_var (id,dims,mt)) =
    case lookUp st id of
        Nothing -> sf
            where 
                sf = insertIfNotLevel id lNum st mv 1
        Just x  -> case x of
            -- same in both cases
            I_VARIABLE(level,_,_,_) -> sf
                where
                    sf = insertIfNotLevel id lNum st mv level
            I_FUNCTION(level,_,_,_) -> sf
                where
                    sf = insertIfNotLevel id lNum st mv level


--  make sure it has not been declared in current scope
-- 1 create new scope for function
-- 2 insert arguments
-- 3 wff_decl for all func decls
-- 4 wff_stmt for all func stmts                    
-- 5 delete the scope
wff_decl lNum st mf@(M_fun (id,args,retType,decls,stmts)) = 
    case lookUp st id of
        Nothing -> sf
            where
                sf = insertIfNotLevel id lNum st mf 1
        Just x -> case x of
            -- same in both cases
            I_VARIABLE(level,_,_,_) -> sf
                where
                    sf = insertIfNotLevel id lNum st mf level
            I_FUNCTION(level,_,_,_) -> sf
                where
                    sf = insertIfNotLevel id lNum st mf level


insertIfNotLevel :: String -> Int -> ST -> M_decl -> Int -> SF (Int,ST)
insertIfNotLevel s lNum st _ 0 = FF $ "M_decl: There is an existant declaration for " ++ show s ++ "\n"
insertIfNotLevel s lNum st (M_var (id,dims,mt)) level = sf
    where 
        sf = if (allMIntExpr st dims)
                then let (lNum',st') = insert lNum st (VARIABLE (id,mt,length dims))
                    in SS (lNum',st')
                else FF $ "M_decl: Incompatible array dimensions for " ++ show id ++ "\n"
-- Incomplete
insertIfNotLevel s lNum st (M_fun (id,args,retType,decls,stmts)) level = sf
    where
        iFuncArgs = map crIFuncArg args
        sf = SS (insert lNum st (FUNCTION (id,iFuncArgs,retType)))


stepIntoAllFunc :: Int -> ST -> [M_decl] -> SF (Int,ST)
stepIntoAllFunc lNum st [] = SS (lNum,st)
stepIntoAllFunc lNum st (d:ds) = sf
    where
        sf1 = stepIntoFunc lNum st d
        sf = if (isSS sf1)
                then let (lNum',st') = fromSS sf1 in stepIntoAllFunc lNum' st' ds 
                else FF $ fromFF sf1
        

stepIntoFunc :: Int -> ST -> M_decl -> SF (Int,ST)
stepIntoFunc lNum st (M_var _) = SS (lNum,st)
stepIntoFunc lNum st (M_fun (id,args,retType,decls,stmts)) = sf    
    where
        -- create new temp scope
        st0 = newScope (L_FUN retType) st
        -- inserts args
        insArgs = map crArgument args
        (lNum1,st1) = allwff_func_args lNum st0 insArgs
        -- inserts decls
        sf1 = allwff_decl lNum1 st1 decls
        -- check each of the func decls
        sf2 = if (isSS sf1)
                then let (lNum',st') = fromSS sf1 in stepIntoAllFunc lNum' st' decls
                else FF $ ""
        -- insert stmts
        sf3 = if (isSS sf2)
                then let (lNum',st') = fromSS sf2 in allwff_stmt lNum' st' stmts 
                else FF $ ""
        sf = if ((isSS sf1) && (isSS sf2) && (isSS sf3))
                then let    
                    (lNum',st') = fromSS sf2
                    -- delete temp scope
                    st'' = delete st'
                    in SS (lNum',st'')
                else FF $ fromFF sf1 ++ fromFF sf2 ++ fromFF sf3













wff_stmt :: Int -> ST -> M_stmt -> SF Int
wff_stmt lNum st (M_ass (id,dims,e)) = 
    case lookUp st id of
        Nothing -> FF $ "M_ass: No existant declaration for " ++ show id --(False,lNum)
        Just x  -> case x of
            I_VARIABLE y    -> sf
                where
                    mt1 = getLookupType x
                    b = withinDims x dims
                    sf1 = wff_expr st e
                    mMt = sameMtype (SS mt1) sf1
                    sf = if (b && (isSS sf1) && (isJust mMt))
                            then SS lNum
                            else FF $ "M_ass: Invalid dimensions for " ++ show id ++ "\n" ++ fromFF sf1
            otherwise       -> FF $ "M_ass: Cannot assign to a function " ++ show id ++ "\n" --(False,lNum)

wff_stmt lNum st (M_while (e,s)) = sf
    where
        sfe = wff_expr st e 
        b = if (isSS sfe)
            then let mt = fromSS sfe in isMBool mt
            else False
        sfs = wff_stmt lNum st s
        sf = if (b && (isSS sfe) && (isSS sfs))
            then sfs
            else FF$ fromFF sfe ++ fromFF sfs



wff_stmt lNum st (M_cond (e,s1,s2)) = sf
    where
        sfe1 = wff_expr st e 
        b = if (isSS sfe1)
            then let mt = fromSS sfe1 in isMBool mt
            else False
        sfs1 = wff_stmt lNum st s1 
        sfs2 = if (isSS sfs1)
            then let lNum1 = fromSS sfs1 in wff_stmt lNum1 st s2
            else FF $ ""
        sf = if (b && (isSS sfs1) && (isSS sfs2))
            then sfs2
            else FF$ fromFF sfe1 ++ fromFF sfs1 ++ fromFF sfs2
        

wff_stmt lNum st (M_read (id,dims)) =
    case lookUp st id of
        Nothing -> FF $ "M_read: No existant declaration for " ++ show id ++ "\n" --(False,lNum)
        Just x  -> case x of
            I_VARIABLE y    -> sf
                where
                    b1 = withinDims x dims 
                    b2 = allMIntExpr st dims 
                    sf = if b1 && b2
                        then SS lNum
                        else FF $ "M_id: Invalid dimensions for " ++ show id ++ "\n"
            otherwise       -> FF $ "M_read: Cannot read into a function " ++ show id ++ "\n"--(False,lNum)

wff_stmt lNum st (M_print e) = sf
    where
        sf1 = wff_expr st e 
        sf = if (isSS sf1)
            then SS lNum
            else FF $ fromFF sf1

wff_stmt lNum st (M_return e) = sf
    where
-- SF M_type
        sf1 = wff_expr st e
        mt = stReturn st
        mMt = sameMtype sf1 (SS mt)
        sf = if ((isSS sf1) && (isJust mMt))
            then SS lNum
            else
                case sf1 of 
                    SS _ -> FF $ "M_return: Expected to return " ++ (show mt) ++ " but returns " ++ show (fromSS sf1) ++ "\n"
                    FF _ -> FF $ fromFF sf1
                
-- SF Int
wff_stmt lNum st (M_block (ds,ss)) = sf
    where
        st0 = newScope L_BLK st
        sf1 = allwff_decl lNum st0 ds
        sf2 = if (isSS sf1)
            then let (lNum1,st1) = fromSS sf1 in stepIntoAllFunc lNum1 st1 ds
            else FF ""
        sf = if (isSS sf2)
            then let (lNum1,st1) = fromSS sf1 in allwff_stmt lNum1 st1 ss 
            else FF $ fromFF sf1 ++ fromFF sf2





-- GOOD 
wff_expr :: ST -> M_expr -> SF M_type
wff_expr st (M_ival n) = SS M_int
wff_expr st (M_rval n) = SS M_real
wff_expr st (M_bval b) = SS M_bool
wff_expr st (M_size (s,n)) = 
    case lookUp st s of
        Nothing     -> FF $ "M_size: No existant declaration for " ++ show s ++ "\n"--(False, M_int)
        Just x      -> case x of
            I_VARIABLE y    -> sf
                where
                    sf = if (checkDim x n)
                        then SS M_int
                        else FF $ "M_size: Invalid dimensions for " ++ show s ++ "\n"
            otherwise       -> FF $ "M_size: Cannot get size of a variable " ++ show s ++ "\n"--(False, M_int)
wff_expr st (M_id (s,es)) = 
    case lookUp st s of
        Nothing -> FF $ "M_id: No existant declaration for " ++ s ++ "\n"--(False, M_int)
        Just x  -> case x of
            (I_VARIABLE y)    -> sf
                where
                    mt = getLookupType x
                    b1 = allMIntExpr st es
                    b2 = withinDims x es
                    sf = if (b1 && b2)
                        then SS mt
                        else FF $ "M_id: Invalid dimensions for " ++ show s ++ "\n"
            otherwise       -> FF $ "M_id: Cannot perform ID on a function " ++ show s ++ "\n"--(False,M_int)

            -- broken
            -- if we get M_fn string, have to make sure that argument number, type and order all match
wff_expr st (M_app (op,es)) = sf
    where
        sf1 = wff_op st op
        -- argument types
        sf2 = evalwff_exprs st es
        sf = if ((isSS sf1) && (isSS sf2))
                then
                    let 
                        mtss = fromSS sf1
                        mts = fromSS sf2
                    in case (mts `elem` mtss) of
                        True -> sf
                            where
                                sf = lookupReturnType st op mts
                        False -> FF $ "M_app: Cannot apply " ++ (show op) ++ " to arguments of type " ++ show (mts) ++ "\n"

                else FF $ fromFF sf1 ++ fromFF sf2

evalwff_exprs :: ST -> [M_expr] -> SF [M_type]
evalwff_exprs st [] = SS []
evalwff_exprs st [e] = sf
    where
        sf1 = wff_expr st e
        sf = if (isSS sf1)
                then let mt = fromSS sf1 in SS [mt]
                else FF $ fromFF sf1
evalwff_exprs st (e:es) = sf
    where
        sf1 = wff_expr st e 
        sf2 = evalwff_exprs st es 
        sf = if (isSS sf1) && (isSS sf2)
                then 
                    let
                        mt = fromSS sf1
                        mts = fromSS sf2
                    in SS (mt:mts)
                else FF $ fromFF sf1 ++ fromFF sf2





wff_op :: ST -> M_operation -> SF [[M_type]]
wff_op st (M_fn s) = 
    case lookUp st s of
        Nothing -> FF $ "M_op: No existant declaration for function " ++ show s ++ "\n"-- (False,[])
        Just x  -> case x of 
            I_FUNCTION(level,label,args,retType)    ->  SS [(getMtypeFromArgs args)]
            otherwise                               ->  FF $ "M_op Cannot call a variable " ++ show s ++ "\n"--(False,[])    
wff_op st op
    | op `elem` [M_lt , M_le , M_gt , M_ge , M_eq , M_add , M_mul , M_sub , M_div , M_neg]                     
                                                                            = SS [[M_int,M_int],[M_real,M_real]]
    | op `elem` [M_and , M_or]                                              = SS [[M_bool,M_bool]]
    | op `elem` [M_not]                                                     = SS [[M_bool]]
    | op `elem` [M_float , M_floor , M_ceil]                                = SS [[M_int],[M_real]]
    | otherwise                                                             = FF $ "Operation " ++ (show op) ++ " does not exist" ++ "\n"-- (False,[])


    