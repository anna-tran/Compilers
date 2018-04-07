module CodeGen where

import IR 
import Data.Maybe

genLabel :: Int -> (Int,String)
genLabel n = (n+1,label)
    where
        label = "label" ++ show n

foldWithLabel :: Int -> (Int-> a -> (Int,String)) -> [a] -> (Int,String)
foldWithLabel lNum func [] = (lNum,"")
foldWithLabel lNum func [x] = (lNum',str)
    where
        (lNum',str) = func lNum x
foldWithLabel lNum func (x:xs) = (lNum2,str)
    where
        (lNum1,str1) = func lNum x 
        (lNum2,str2) = foldWithLabel lNum1 func xs 
        str = str1 ++ str2

genIProg :: I_prog -> String
genIProg (IPROG (fbodies,nVar,arrDims,stmts)) = 
    "LOAD_R %sp\n" ++
    "LOAD_R %sp\n" ++
    "STORE_R %fp\n" ++
    "ALLOC " ++ show nVar ++ "\n" ++ -- allocate space for variables
    "LOAD_I " ++ show (nVar+1) ++ "\n" ++   -- initializes deallocation counter    
    arrayCode ++ -- allocate space for arrays
    stmtCode ++
    "LOAD_R %fp\n" ++                       -- deallocate local storage
    "LOAD_O " ++ show (nVar+1) ++ "\n" ++
    "APP NEG\n" ++
    "ALLOC_S\n" ++    
    "STORE_R %fp\n" ++                      -- restore the caller frame pointer    
    "HALT\n\n" ++
    funCode
    where
        arrayCode = (foldl (\acc arrDim -> (genArray nVar arrDim) ++ acc) "" arrDims)
        (lNum,stmtCode) = foldWithLabel 0 genIStmt stmts
        (lNum',funCode) = foldWithLabel lNum genIFbody fbodies

        


-- offset = offset of array in local var from fp
genArray :: Int -> (Int,[I_expr]) -> String
genArray nVar (offset,[]) = ""
genArray nVar (offset,dims) =
    "LOAD_R %sp\n" ++
    "LOAD_R %fp\n" ++
    "STORE_O " ++ show offset ++ "\n" ++ 
    (foldl (\acc e -> (genIExpr e) ++ acc) "" dims) ++ -- put all dimensions of the array on the stack
    (genArrayHeaders nDims offset) ++
    (genArrayStorage nDims offset) ++       -- TOS has the number of slots to allocate to array based on dimensions

    -- now sum up the total amount of space needed for this array
    "LOAD_R %fp\n" ++
    "LOAD_O " ++ show (nVar+1) ++ "\n" ++   -- load deallocation counter
    "LOAD_I " ++ show nDims ++ "\n" ++      -- load # of array bounds
    "APP ADD\n" ++
    "APP ADD\n" ++
    
    "LOAD_R %fp" ++ 
    "STORE_O " ++ show (nVar+1) ++ "\n" ++  -- assign new value to deallocaton counter
    "ALLOC_S\n"                             -- allocate required storage for array
    where
        nDims = length dims

-- x[3][2][1] is stored on stack as 3 | 2 | 1 | sp
genArrayHeaders :: Int -> Int -> String
genArrayHeaders 0 offset = ""
genArrayHeaders 1 offset =
    "LOAD_R %fp\n" ++
    "LOAD_O " ++ show offset ++ "\n" ++
    "STORE_O 1\n"    
genArrayHeaders nDims offset = 
    "LOAD_R %fp\n" ++
    "LOAD_O " ++ show offset ++ "\n" ++
    "STORE_O " ++ show nDims ++ "\n" ++
    genArrayHeaders (nDims-1) offset

genArrayStorage :: Int -> Int -> String
genArrayStorage 0 offset = ""
genArrayStorage 1 offset = 
    "LOAD_R %fp\n" ++
    "LOAD_O " ++ show offset ++ "\n" ++
    "LOAD_O 1\n"
genArrayStorage nDims offset =
    (genArrayStorage (nDims-1) offset) ++
    "LOAD_R %fp\n" ++
    "LOAD_O " ++ show offset ++ "\n" ++
    "LOAD_O " ++ show nDims ++ "\n" ++
    "APP MUL\n"



genIFbody :: Int -> I_fbody -> (Int,String)
genIFbody lNum (IFUN (label,fbodies,nVar,nArg,arrDims,stmts)) = (lNum3,
    label ++ ":\n" ++
    "LOAD_R %sp\n" ++
    "STORE_R %fp\n" ++                      -- set new FP to top stack element
    "ALLOC " ++ show nVar ++ "\n" ++        -- allocate nVar void cells
    "LOAD_I " ++ show (nVar+1) ++ "\n" ++   -- initializes deallocation counter

    arrayCode ++
    stmtCode ++                             -- value of return loaded

    "LOAD_R %fp\n" ++                       -- return from a function call
    "STORE_O " ++ show (-(nArg+3)) ++ "\n" ++   -- write the return value to the first argument slot on the stack
    "LOAD_R %fp\n" ++                       -- write the return pointer to the second argument slot
    "LOAD_O 0\n" ++
    "LOAD_R %fp\n" ++
    "STORE_O " ++ show (-(nArg+2)) ++ "\n" ++

    "LOAD_R %fp\n" ++                       -- deallocate local storage
    "LOAD_O " ++ show (nVar+1) ++ "\n" ++
    "APP NEG\n" ++
    "ALLOC_S\n" ++

    "STORE_R %fp\n" ++                      -- restore the caller frame pointer
    "ALLOC " ++ show (-nArg) ++ "\n" ++     -- clean up the arguments
    "JUMP_S\n\n" ++                           -- do jump to the return code pointer
    funCode 
    )
    where
        (lNum1,label1) = genLabel lNum
        arrayCode = (foldl (\acc arrDim -> (genArray nVar arrDim) ++ acc) "" arrDims)
        (lNum2,stmtCode) = foldWithLabel lNum1 genIStmt stmts
        (lNum3,funCode) = foldWithLabel lNum2 genIFbody fbodies

genIStmt :: Int -> I_stmt -> (Int,String) 
genIStmt lNum (IASS (level,offset,arrIndices,ie)) = (lNum,
    exprCode ++     -- push the expr value onto the stack
    arrCode ++      -- push the correct pointer and offset from fp onto the stack
    "STORE_OS\n"
    ) where
        exprCode = genIExpr ie
        arrCode = genArrayAccess level offset arrIndices

genIStmt lNum (IWHILE (ie,is)) = (lNum3,
    label1 ++ ":\n" ++
    exprCode ++
    "JUMP_C " ++ label2 ++ "\n" ++
    stmtCode ++
    "JUMP " ++ label1 ++ "\n" ++
    label2 ++ ":\n"
    ) where
        exprCode = genIExpr ie
        (lNum1,label1) = genLabel lNum
        (lNum2,label2) = genLabel lNum1
        (lNum3,stmtCode) = genIStmt lNum2 is
        
genIStmt lNum (ICOND (ie,is1,is2)) = (lNum4,
    exprCode ++ 
    "JUMP_C " ++ label1 ++ "\n" ++  -- if false, jump to false label1
    stmtCode1 ++                    -- if true, do this
    "JUMP " ++ label2 ++ "\n" ++    -- jump to end
    label1 ++ ":\n" ++
    stmtCode2 ++
    label2 ++ ":\n"                 -- straight to end
    ) where
        exprCode = genIExpr ie
        (lNum1,label1) = genLabel lNum
        (lNum2,label2) = genLabel lNum1
        (lNum3,stmtCode1) = genIStmt lNum2 is1
        (lNum4,stmtCode2) = genIStmt lNum3 is2

genIStmt lNum (IREAD_F (level,offset,arrIndices)) = (lNum,
    "READ_F\n" ++
    arrayCode ++
    "STORE_OS\n"
    ) where 
        arrayCode = genArrayAccess level offset arrIndices

genIStmt lNum (IREAD_I (level,offset,arrIndices)) = (lNum,
    "READ_I\n" ++
    arrayCode ++
    "STORE_OS\n"
    ) where 
        arrayCode = genArrayAccess level offset arrIndices

genIStmt lNum (IREAD_B (level,offset,arrIndices)) = (lNum,
    "READ_B\n" ++
    arrayCode ++
    "STORE_OS\n"
    ) where 
        arrayCode = genArrayAccess level offset arrIndices

genIStmt lNum (IPRINT_F ie) = (lNum,
    exprCode ++ 
    "PRINT_F\n"
    ) where 
        exprCode = genIExpr ie

genIStmt lNum (IPRINT_I ie) = (lNum,
    exprCode ++ 
    "PRINT_I\n"
    ) where 
        exprCode = genIExpr ie

genIStmt lNum (IPRINT_B ie) = (lNum,
    exprCode ++ 
    "PRINT_B\n"
    ) where 
        exprCode = genIExpr ie

-- put expression on top of the stack
genIStmt lNum (IRETURN ie) = (lNum,exprCode) 
    where 
        exprCode = genIExpr ie

genIStmt lNum (IBLOCK (fbodies,nVar,arrDims,stmts)) = (lNum2,
    "LOAD_R %fp\n" ++                       -- save the old frame pointer as access link
    "ALLOC 2\n" ++                          -- 2 void cells to be consistent with finding access link
    "LOAD_R %sp\n" ++
    "STORE_R %fp\n" ++                      -- set new FP to top stack element
    "ALLOC " ++ show nVar ++ "\n" ++        -- allocate nVar void cells
    
    "LOAD_I " ++ show (nVar+3) ++ "\n" ++   -- initializes deallocation counter, +2 to get rid of void cells

    arrayCode ++
    stmtCode ++                             

    "LOAD_R %fp\n" ++                       -- deallocate local storage
    "LOAD_O " ++ show (nVar+1) ++ "\n" ++
    "APP NEG\n" ++
    "ALLOC_S\n" ++                          -- now old frame pointer on top of stack

    "STORE_R %fp\n\n" ++                    -- restore the caller frame pointer
    
    funCode 
    )
    where
        arrayCode = (foldl (\acc arrDim -> (genArray nVar arrDim) ++ acc) "" arrDims)
        (lNum1,stmtCode) = foldWithLabel lNum genIStmt stmts
        (lNum2,funCode) = foldWithLabel lNum1 genIFbody fbodies           
        



-- push the correct sp and offset m onto the stack
genArrayAccess :: Int -> Int -> [I_expr] -> String
genArrayAccess level offset arrIndices = 
    case arrSlot of 
        Nothing -> 
                    jumpToLevel level ++                -- push the correct level fp onto the stack
                    "LOAD_I " ++ show offset ++ "\n"    -- push offset onto the stack
        Just accessCode -> 
                    jumpToLevel level ++                -- push the correct level fp onto the stack
                    "LOAD_O " ++ show offset ++ "\n" ++ -- top of stack has the pointer to array header
                    "LOAD_I " ++ show nDims ++ "\n" ++
                    accessCode ++
                    "APP ADD\n"                         -- #dims (accounts for the header) + offset of slot
    where
        nDims = length arrIndices
        arrSlot = genArraySlot level offset nDims arrIndices



-- if there is at least one dimension, push the offset of the destination slot 
-- relative to the sp' of the array a onto the stack
genArraySlot :: Int -> Int -> Int -> [I_expr] -> Maybe String
genArraySlot level offset nDims [] = Nothing
genArraySlot level offset nDims [arrIndex] = 
    Just $ genIExpr arrIndex -- puts the location of the slot onto the stack
genArraySlot level offset nDims (ai:ais) =
    Just $ 
    jumpToLevel level ++
    "LOAD_O " ++ show offset ++ "\n" ++
    "LOAD_O " ++ show (nDims - length ais) ++ "\n" ++ 
    "LOAD_I 1\n" ++     
    "APP SUB\n" ++      -- (d1-1)
    (genIExpr ai) ++
    "APP MUL\n" ++      -- (d1-1) * r
    fromJust (genArraySlot level offset nDims ais) ++
    "APP ADD\n"         -- (d1-1) * r + s



-- push value on top of the stack
genIExpr :: I_expr -> String
genIExpr (IINT n) = "LOAD_I " ++ show n ++ "\n"
genIExpr (IREAL n) = "LOAD_F " ++ show n ++ "\n"
genIExpr (IBOOL b) = "LOAD_B " ++ show b ++ "\n"
genIExpr (IID (level,offset,arrIndices)) = 
    arrayCode ++
    "LOAD_OS\n"
    where
        arrayCode = genArrayAccess level offset arrIndices
genIExpr (IAPP (op,ies)) =
    exprsCode ++
    opCode
    where
        exprsCode = foldl (\acc ie -> (genIExpr ie) ++ acc) "" ies
        opCode = genIOpn op

genIExpr (ISIZE (level,offset,dimNum)) = 
    jumpToLevel level ++
    "LOAD_O " ++ show offset ++ "\n" ++
    "LOAD_O " ++ show dimNum ++ "\n"



-- get the fp of the given level
jumpToLevel :: Int -> String 
jumpToLevel level = 
    "LOAD_R %fp\n" ++
    foldl1 (++) (replicate level "LOAD_O -2\n")


genIOpn :: I_opn -> String
genIOpn (ICALL (label,level)) = 
    -- load args
    "ALLOC 1\n" ++
    jumpToLevel level ++ 
    "LOAD_R %fp\n" ++
    "LOAD_R %cp\n" ++
    "JUMP " ++ label ++ "\n"
genIOpn IADD = "APP ADD\n"
genIOpn IMUL = "APP MUL\n"
genIOpn ISUB = "APP SUB\n"
genIOpn IDIV = "APP DIV\n"
genIOpn INEG = "APP NEG\n"
genIOpn ILT = "APP LT\n"
genIOpn ILE = "APP LE\n"
genIOpn IGT = "APP GT\n"
genIOpn IGE = "APP GE\n"
genIOpn IEQ = "APP EQ\n"    
genIOpn IADD_F = "APP ADD_F\n"
genIOpn IMUL_F = "APP MUL_F\n"
genIOpn ISUB_F = "APP SUB_F\n"
genIOpn IDIV_F = "APP DIV_F\n"
genIOpn INEG_F = "APP NEG_F\n"
genIOpn ILT_F = "APP LT_F\n"
genIOpn ILE_F = "APP LE_F\n"
genIOpn IGT_F = "APP GT_F\n"
genIOpn IGE_F = "APP GE_F\n"
genIOpn IEQ_F = "APP EQ_F\n"
genIOpn INOT = "APP NOT\n"
genIOpn IAND = "APP AND\n"
genIOpn IOR = "APP OR\n"
genIOpn IFLOAT = "APP FLOAT\n"
genIOpn ICEIL = "APP CEIL\n"
genIOpn IFLOOR = "APP FLOOR\n"