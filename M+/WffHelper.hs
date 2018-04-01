module WffHelper where

import SymbolTypes
import SymbolTable
import AstM
import Data.Maybe
 




        


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

sameMtype :: M_Type -> SF (M_type,a) -> Maybe M_type
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

lookupReturnType :: ST -> M_operation -> [M_type] -> SF M_type
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
    | op `elem` [M_float , M_floor , M_ceil]
        = if (isSS sfm)
            then SS M_real
            else sfm    
    | op `elem` [M_add , M_mul , M_sub , M_div , M_neg] = sfm         
    | otherwise = FF $ "Operation " ++ (show op) ++ " does not exist\n"
    where
        sfm = allSameMtype mts


allSameMtype :: [M_type] -> SF M_type
allSameMtype [] = FF $ "No M_type given!"
allSameMtype [m] = SS m
allSameMtype (m:ms) = sf
    where
        sf1 = allSameMtype ms
        sf = if (isSS sf1)
            then let mt = fromSS sf1 in
                if mt == m
                then SS m
                else FF $ "Not of the same type: " ++ (show m) ++ ", " ++ (show mt) ++ "\n"
            else sf1
