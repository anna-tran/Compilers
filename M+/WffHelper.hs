module WffHelper where

import SymbolTypes
import SymbolTable
import AstM
import Data.Maybe
import IR 




        


getLookupType :: SYM_I_DESC -> M_type
getLookupType (I_VARIABLE(level,offset,mtype,dim)) = mtype
getLookupType (I_FUNCTION(level,label,args,retType)) = retType



    
checkDim :: SYM_I_DESC -> Int -> Bool
checkDim (I_VARIABLE(level,offset,mtype,dim)) n
    | n>=0 && n < dim = True
    | otherwise                = False
checkDim x n = False

withinDims :: SYM_I_DESC -> [M_expr] -> Bool
withinDims (I_VARIABLE(level,offset,mtype,dim)) es = b
    where
        n = length es
        b = n >= 0 && n <= dim
withinDims x es = False

exactDims :: SYM_I_DESC -> [M_expr] -> Bool
exactDims (I_VARIABLE(level,offset,mtype,dim)) es = b
    where
        n = length es
        b = n == dim
exactDims x es = False

sameMtype :: M_type -> SF (M_type,a) -> Maybe M_type
sameMtype x (SS (y,_))
    | x == y        = Just x
    | otherwise     = Nothing
sameMtype x ff   = Nothing


isMBool :: M_type -> Bool
isMBool (M_bool) = True
isMBool x = False

isMInt :: M_type -> Bool
isMInt (M_int) = True
isMInt x = False

crArgument :: (String,Int,M_type) -> SYM_DESC
crArgument (id,nDims,mtype) = ARGUMENT (id,mtype,nDims)



findInDecls :: [M_decl] -> String -> M_decl
findInDecls [] id = error $ "Could not find declaration " ++ (show id) ++ "\n"
findInDecls (d@(M_var (id',_,_)):ds) id
    | id' == id = d 
    | otherwise = findInDecls ds id 
findInDecls (d@(M_fun (id',_,_,_,_)):ds) id
    | id' == id = d 
    | otherwise = findInDecls ds id 

crIFuncArg :: (String,Int,M_type) -> (M_type,Int)
crIFuncArg (id,nDims,mt) = (mt,nDims)


getMtypeFromArgs :: [(M_type,Int)] -> [M_type]
getMtypeFromArgs [] = []
getMtypeFromArgs ((mt,i):rest) = mt : (getMtypeFromArgs rest)

lookupReturnType :: ST -> M_operation -> [(M_type,Int)] -> SF M_type
lookupReturnType st (M_fn s) mts =
    case lookUp st s of
        Nothing -> FF $ "No existant declaration for function " ++ show s ++ "\n"
        Just x -> case x of 
            I_FUNCTION(_,_,_,mtype) -> SS mtype
            otherwise -> FF $ "Cannot get return type of a variable " ++ show s ++ "\n"

lookupReturnType st op mts 
    | op `elem` [M_lt , M_le , M_gt , M_ge , M_eq , M_and , M_or , M_not] 
        = if (isSS sfm)
            then SS M_bool
            else sfm
    | op `elem` [M_float]
        = if (isSS sfm)
            then SS M_real
            else sfm
    | op `elem` [M_floor , M_ceil]
        = if (isSS sfm)
            then SS M_int
            else sfm    
    | op `elem` [M_add , M_mul , M_sub , M_div , M_neg] = sfm         
    | otherwise = FF $ "Operation " ++ (show op) ++ " does not exist\n"
    where
        sfm = allSameMtype mts


allSameMtype :: [(M_type,Int)] -> SF M_type
allSameMtype [] = FF $ "No M_type given!"
allSameMtype [(m,d)] = SS m
allSameMtype ((m,d):ms) = sf
    where
        sf1 = allSameMtype ms
        sf = if (isSS sf1)
            then let mt = fromSS sf1 in
                if mt == m
                then SS m
                else FF $ "Not of the same type: " ++ (show m) ++ ", " ++ (show mt) ++ "\n"
            else sf1



crEmptyIProg :: I_prog
crEmptyIProg = IPROG ([],0,[],[])





trIOpn :: ST -> M_operation -> M_type -> I_opn
trIOpn st (M_fn id) _ = ICALL (label,level)
    where
        (I_FUNCTION(level,label,argTypes,mtype)) = fromJust $ lookUp st id
trIOpn _ (M_add) M_int = IADD
trIOpn _ (M_add) M_real = IADD_F
trIOpn _ (M_sub) M_int = ISUB
trIOpn _ (M_sub) M_real = ISUB_F
trIOpn _ (M_mul) M_int = IMUL
trIOpn _ (M_mul) M_real = IMUL_F
trIOpn _ (M_div) M_int = IDIV
trIOpn _ (M_div) M_real = IDIV_F
trIOpn _ (M_neg) M_int = INEG
trIOpn _ (M_neg) M_real = INEG_F
trIOpn _ (M_lt) M_int = ILT
trIOpn _ (M_lt) M_real = ILT_F
trIOpn _ (M_le) M_int = ILE
trIOpn _ (M_le) M_real = ILE_F
trIOpn _ (M_gt) M_int = IGT
trIOpn _ (M_gt) M_real = IGT_F
trIOpn _ (M_ge) M_int = IGE
trIOpn _ (M_ge) M_real = IGE_F
trIOpn _ (M_eq) M_int = IEQ
trIOpn _ (M_eq) M_real = IEQ_F
trIOpn _ (M_not)    _ = INOT
trIOpn _ (M_and)    _ = IAND
trIOpn _ (M_or)     _ = IOR
trIOpn _ (M_float)  _ = IFLOAT
trIOpn _ (M_floor)  _ = IFLOOR
trIOpn _ (M_ceil)   _ = ICEIL
trIOpn _ op mt = error $ "Cannot produce an I_opn " ++ show op ++ " with type " ++ show mt

getM_idID :: M_expr -> String
getM_idID (M_id (id,dims)) = id
getM_idID x = error $ "Cannot get id from expression " ++ show x
getM_idDims :: M_expr -> [M_expr]
getM_idDims (M_id (id,dims)) = dims
getM_idDims x = error $ "Cannot get dimensions from expression " ++ show x
