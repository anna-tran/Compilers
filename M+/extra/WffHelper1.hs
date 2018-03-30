

























-- module WffHelper where

    import SymbolTypes
    import SymbolTable
    import AstM
    
    
    
    
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
    
    
    -- getOpType :: ST -> M_operation -> [M_expr] -> [M_type] -> M_type
    -- getOpType st op es expectedT
    --     | op `elem` [M_add , M_mul , M_sub , M_div , M_neg]
    --         && b
    --         && mt `elem` expectedT
    --         = mt
    --     | b && mt `elem` expectedT
    --         = mt
    --     | otherwise = error $ "Operation " ++ (show op) ++ " cannot be applied to given arguments"
    --     where
    --         (b,mt) = sameMtypeL st es
    
    -- sameMtypeL :: ST -> [M_expr] -> (Bool,M_type)
    -- sameMtypeL st [] = (False,M_int)
    -- sameMtypeL st [x] = (b,mt)
    --     where
    --         (b,mt) = wff_expr st x
    -- sameMtypeL st (e:es) = (b,mt)
    --     where
    --         (b1,mt1) = wff_expr st e 
    --         (b2,mt2) = sameMtypeL es 
    --         (b3,mt) = sameMtype mt1 mt2
    --         b = b1 && b2 && b3
    
    sameMtype :: M_type -> M_type -> (Bool,M_type)
    sameMtype x y 
        | x == y        = (True, x)
        | otherwise     = (False, x)
    
    isMBool :: M_type -> Bool
    isMBool (M_bool) = True
    isMBool x = False
    
    isMInt :: M_type -> Bool
    isMInt (M_int) = True
    isMInt x = False
    
    -- declToSymDesc :: M_decl -> SYM_DESC
    -- declToSymDesc (M_var (id,dims,mtype)) = VARIABLE (id,mtype,length dims)
    -- declToSymDesc (M_fun (id,args,retType,decls,stmts)) = FUNCTION (id,args,retType)
    
    
    crArgument :: (String,Int,M_type) -> SYM_DESC
    crArgument (id,nDims,mtype) = ARGUMENT (id,mtype,nDims)
    
    
    
    findInDecls :: [M_decl] -> String -> M_decl
    findInDecls [] id = error $ "Could not find declaration " ++ (show id)
    findInDecls (d@(M_var (id',_,_)):ds) id
        | id' == id = d 
        | otherwise = findInDecls ds id 
    findInDecls (d@(M_fun (id',_,_,_,_)):ds) id
        | id' == id = d 
        | otherwise = findInDecls ds id 
    
    crIFuncArg :: (String,Int,M_type) -> (M_type,Int)
    crIFuncArg (id,nDims,mt) = (mt,nDims)