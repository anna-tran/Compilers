module IR where

data I_prog  = IPROG    ([I_fbody],Int,[(Int,[I_expr])],[I_stmt])
                deriving (Eq, Ord, Read)        

    -- a program node consists of 
    --   (a) the list of functions declared
    --   (b) the number of local variables
    --   (c) a list of array specifications (<offset>,<list of bounds>)
    --   (d) the body: a list of statements
data I_fbody = IFUN     (String,[I_fbody],Int,Int,[(Int,[I_expr])],[I_stmt])
                deriving (Eq, Ord, Read)        

    -- a function node consists of 
    --   (a) the label given to the function
    --   (b) the list of local functions declared
    --   (c) the number of local variables
    --   (d) the number of arguments
    --   (c) a list of array specifications (<offset>,<list of bounds>)
    --   (d) the body: a list of statements
data I_stmt = IASS      (Int,Int,[I_expr],I_expr)
                 -- and assignment has argments (<level>,<offset>,<array indices>,expr)
            | IWHILE    (I_expr,I_stmt)
            | ICOND     (I_expr,I_stmt,I_stmt)
            | IREAD_F   (Int,Int,[I_expr])
            | IREAD_I   (Int,Int,[I_expr])
            | IREAD_B   (Int,Int,[I_expr])
            | IPRINT_F  I_expr
            | IPRINT_I  I_expr
            | IPRINT_B  I_expr
            | IRETURN   I_expr
            | IBLOCK    ([I_fbody],Int,[(Int,[I_expr])],[I_stmt])
            deriving (Eq, Ord, Read)        
            
	         -- a block consists of 
    		 -- (a) a list of local functions
	    	 -- (b) the number of local varibles declared
    		 -- (c) a list of array declarations
	    	 -- (d) the body: a lst of statements
data I_expr = IINT      Int
            | IREAL     Float
            | IBOOL     Bool
            | IID       (Int,Int,[I_expr])   
	         --  identifier (<level>,<offset>,<array indices>)
            | IAPP      (I_opn,[I_expr])
            | ISIZE     (Int,Int,Int)
            deriving (Eq, Ord, Read)        
            
	         --   isize(<level>,<offset>,<which dimension>)
	    	 --   level and offset identify which array the last integer 
		     --   tells you which dimension you want to look at!!
data I_opn = ICALL (String,Int)
           | IADD_F | IMUL_F | ISUB_F | IDIV_F | INEG_F
           | ILT_F  | ILE_F  | IGT_F  | IGE_F  | IEQ_F   -- operations for floats
           | IADD | IMUL | ISUB | IDIV | INEG
           | ILT  | ILE  | IGT  | IGE  | IEQ 
           | INOT | IAND | IOR | IFLOAT | ICEIL |IFLOOR
            deriving (Eq,Ord,Read,Show)

instance Show I_prog where
    show (IPROG (fbodies,nVar,arrDims,ss)) = indent $ "IPROG \n([" ++ showMyList fbodies ++ "],\n" ++ show nVar ++ ",\n[" ++ showMyList arrDims ++ "],\n[" ++ showMyList ss ++ "]"

instance Show I_fbody where
    show (IFUN (label,fbodies,nVar,nArg,arrDims,ss)) = indent $ "IFUN \n(" ++ show label ++ ",\n[" ++ showMyList fbodies ++ "],\n" ++ show nVar ++ ",\n" ++
                                                            show nArg ++ ",\n[" ++ showMyList arrDims ++ "],\n[" ++ showMyList ss ++ "]"

instance Show I_stmt where
    show (IASS (lvl,off,ind,e)) = indent $ "IASS (" ++ show lvl ++ "," ++ show off ++ "," ++ show ind ++ ",\n" ++ show e ++ ")"
    show (IWHILE (e,s)) = indent $ "IWHILE \n(" ++ show e ++ ",\n" ++ show s ++ ")"
    show (ICOND (e,s1,s2)) = indent $ "ICOND \n(" ++ show e ++ ",\n" ++ show s1 ++ ",\n" ++ show s2 ++ ")"
    show (IREAD_F (lvl,off,es)) = "IREAD_F (" ++ show lvl ++ "," ++ show off ++ "," ++ show es ++ ")"
    show (IREAD_I (lvl,off,es)) = "IREAD_I (" ++ show lvl ++ "," ++ show off ++ "," ++ show es ++ ")"
    show (IREAD_B (lvl,off,es)) = "IREAD_B (" ++ show lvl ++ "," ++ show off ++ "," ++ show es ++ ")"
    show (IPRINT_F e) = "IPRINT_F (" ++ show e ++ ")"
    show (IPRINT_I e) = "IPRINT_I (" ++ show e ++ ")"
    show (IPRINT_B e) = "IPRINT_B (" ++ show e ++ ")"
    show (IRETURN e) = "IRETURN (" ++ show e ++ ")"
    show (IBLOCK (fbs,nVar,dims,ss)) = indent $ "IBLOCK \n([" ++ showMyList fbs  ++ "],\n" ++ show nVar  ++ ",\n[" ++ showMyList dims  ++ "],\n[" ++ showMyList ss ++ "])"
    
instance Show I_expr where
    show (IINT n)   = "IINT " ++ show n
    show (IREAL n)  = "IREAL " ++ show n
    show (IBOOL b)  = "IBOOL " ++ show b 
    show (IID (level,offset,dims)) = "IID (" ++ show level ++ "," ++ show offset ++ "," ++ show dims ++ ")"
    show (ISIZE (level,offset,dims)) = "ISIZE (" ++ show level ++ "," ++ show offset ++ "," ++ show dims ++ ")"
    show (IAPP (op,ies)) = indent $ "IAPP (" ++ show op ++ ",\n[" ++ showMyList ies ++ "])"
    

showMyList :: (Show a) => [a] -> String
showMyList [] = ""
showMyList [a] = (show a)
showMyList (x:xs) = (show x) ++ ",\n" ++ (showMyList xs)


addTabs :: Int -> String
addTabs 0 = ""
addTabs n = "    " ++ (addTabs (n-1))

indent :: String -> String
indent [] = ""
indent (t:ts) 
    | t == '\n'     = t:((addTabs 1) ++ (indent ts))
    | otherwise     = t:((indent ts))
    