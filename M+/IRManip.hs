module IRManip where
import IR
import SymbolTypes
import SymbolTable

crEmptyIProg :: I_prog
crEmptyIProg = IPROG ([],0,[],[])


trIFbody :: ST -> M_decl -> I_fbody
trIFbody _ (M_var _) = error "Cannot produce an intermediate function body from a variable!"
trIFbody st (M_fun (id,args,retType,decls,stmts)) =
    where 
        (I_FUNCTION(level,label,argTypes,mtype)) = lookUp st id
        fbodies = map (trIFbody st


data I_fbody = IFUN     (String,[I_fbody],Int,Int,[(Int,[I_expr])],[I_stmt])
    -- a function node consists of 
    --   (a) the label given to the function
    --   (b) the list of local functions declared
    --   (c) the number of local variables
    --   (d) the number of arguments
    --   (c) a list of array specifications (<offset>,<list of bounds>)
    --   (d) the body: a list of statements


trIStmt :: ST -> M_stmt -> M_type -> I_stmt
trIStmt st (M_ass (id,dims,e)) _ = IASS (level,offset,indices,ie)
    where
        (I_VARIABLE (level,offset,mtype,dim)) = lookUp st id
        indices = map (trIExpr st) dims
        ie = trIExpr st e 
trIStmt st (M_while (e,s)) _ = IWHILE (ie,is)
    where
        ie = trIExpr st e 
        is = trIStmt st s
trIStmt st (M_cond (e,s1,s2)) _ = ICOND (ie,is1,is2)
    where 
        ie = trIExpr st e 
        is1 = trIStmt st s1
        is2 = trIStmt st s2
        

trIStmt st (M_read e) mtype = 
    case e of 
        (M_id (id,es)) -> case mtype of 
            M_int -> IREAD_I (level,offset,ies)
            M_real -> IREAD_F (level,offset,ies)
            M_bool -> IREAD_B (level,offset,ies)
            where
                (I_VARIABLE (level,offset,mtype',dim)) = lookUp st id
                ies = map (trIExpr st) es???????

        otherwise -> error $ "Cannot read from anything other than an id"

trIStmt st (M_print e) mtype =
    case mtype of 
        M_int -> IPRINT_I ie
        M_real -> IPRINT_F ie
        M_bool -> IPRINT_B ie
        where
            ie = trIExpr e    

trIStmt st (M_return e) _ = ie
    where
        ie = trIExpr e

trIStmt st (M_block (ds,ss)) _ = IBLOCK (fbodies,nVar,arrDecs,is)
    where
        fbodies = map (trIFbody st) ds 
        (nVar,arrDecs) = trIVars st ds
        is = map trIStmt ss




trIVars :: ST -> [M_decl] -> (Int,[(Int,[I_expr])])
trIVars st [] = (0,[])
trIVars st ((M_fun _):rest) = trIVars st rest
trIVars st ((M_var (id,dims,mtype)):rest) = (nVar+1,(offset,ies):arrDecs)
    where
        (I_VARIABLE (level,offset,mtype',dim)) = lookUp st id
        ies = map (trIExpr st) dims
        (nVar,arrDecs) = trIVars st rest
             



trIExpr :: ST -> M_expr -> I_expr
trIExpr _ (M_ival n)        = IINT n
trIExpr _ (M_rval n)        = IREAL n 
trIExpr _ (M_bval b)        = IBOOL b 
trIExpr st (M_size (id,n))  = ISIZE (level,offset,n)
    where
        (I_VARIABLE (level,offset,mtype,dim)) = lookUp st id
trIExpr st (M_id (id,es))   = IID (level,offset,indices)
    where
        (I_VARIABLE (level,offset,mtype,dim)) = lookUp st id
        indices = mapM (trIExpr v) es
-- pass to this a list of one M_expr -- The expected return type from the operation
trIExpr st (M_app (op,es))   = IAPP (iopn,ies)
    where
        ies = map (trIExpr st) es
        iopn = trIOpn st op (trMType (head es))

        

             
trMType :: M_expr -> M_type
trMType (M_ival n) = M_int
trMType (M_rval n) = M_real 
trMType (M_bval b) = M_bool



trIOpn :: ST -> M_operation -> M_type -> I_opn
trIOpn (M_fn id) _ = ICALL (label,level)
    where
        (I_FUNCTION(level,label,argTypes,mtype)) = lookUp st id
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
trIOpn _ (M_not) M_bool = INOT
trIOpn _ (M_and) M_bool = IAND
trIOpn _ (M_or) M_bool = IOR
trIOpn _ (M_float) _ = IFLOAT
trIOpn _ (M_floor) _ = IFLOOR
trIOpn _ (M_ceil) _ = ICEIL

 | M_add | M_mul | M_sub | M_div | M_neg
                 | M_lt | M_le | M_gt | M_ge | M_eq | M_not | M_and | M_or
                 | M_float | M_floor | M_ceil
data I_opn = ICALL (String,Int)
           | IADD_F | IMUL_F | ISUB_F | IDIV_F | INEG_F
           | ILT_F  | ILE_F  | IGT_F  | IGE_F  | IEQ_F   -- operations for floats
           | IADD | IMUL | ISUB | IDIV | INEG
           | ILT  | ILE  | IGT  | IGE  | IEQ 
           | INOT | IAND | IOR | IFLOAT | ICEIL |IFLOOR