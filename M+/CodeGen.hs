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
    "\tLOAD_R %sp\n" ++
    "\tLOAD_R %sp\n" ++
    "\tSTORE_R %fp\n" ++
    "\tALLOC " ++ show nVar ++ "\n" ++ -- allocate space for variables
    "\tLOAD_I " ++ show (nVar+1) ++ "\n" ++   -- initializes deallocation counter    
    arrayCode ++ -- allocate space for arrays
    stmtCode ++
    "\tLOAD_R %fp\n" ++                       -- deallocate local storage
    "\tLOAD_O " ++ show (nVar+1) ++ "\n" ++
    "\tAPP NEG\n" ++
    "\tALLOC_S\n" ++    
    "\tSTORE_R %fp\n" ++                      -- restore the caller frame pointer    
    "\tHALT\n\n" ++
    funCode
    where
        arrayCode = (foldl (\acc arrDim -> acc ++ (genArray nVar arrDim)) "" arrDims)
        (lNum,stmtCode) = foldWithLabel 0 genIStmt stmts
        (lNum',funCode) = foldWithLabel lNum genIFbody fbodies

        


-- offset = offset of array in local var from fp
genArray :: Int -> (Int,[I_expr]) -> String
genArray nVar (offset,[]) = ""
genArray nVar (offset,dims) =
    "\tLOAD_R %sp\n" ++
    "\tLOAD_R %fp\n" ++
    "\tSTORE_O " ++ show offset ++ "\n" ++ 
    (foldl (\acc e -> acc ++ (genIExpr e)) "" dims) ++ -- put all dimensions of the array on the stack
    (genArrayHeaders nDims offset) ++
    (genArrayStorage nDims offset) ++       -- TOS has the number of slots to allocate to array based on dimensions

    -- now sum up the total amount of space needed for this array
    "\tLOAD_R %fp\n" ++
    "\tLOAD_O " ++ show (nVar+1) ++ "\n" ++   -- load deallocation counter
    "\tLOAD_I " ++ show nDims ++ "\n" ++      -- load # of array bounds
    "\tAPP ADD\n" ++
    "\tAPP ADD\n" ++
    
    "\tLOAD_R %fp\n" ++ 
    "\tSTORE_O " ++ show (nVar+1) ++ "\n" ++  -- assign new value to deallocaton counter
    "\tALLOC_S\n"                             -- allocate required storage for array
    where
        nDims = length dims

-- x[3][2][1] is stored on stack as 3 | 2 | 1 | sp
genArrayHeaders :: Int -> Int -> String
genArrayHeaders 0 offset = ""
genArrayHeaders 1 offset =
    "\tLOAD_R %fp\n" ++
    "\tLOAD_O " ++ show offset ++ "\n" ++
    "\tSTORE_O 1\n"    
genArrayHeaders nDims offset = 
    "\tLOAD_R %fp\n" ++
    "\tLOAD_O " ++ show offset ++ "\n" ++
    "\tSTORE_O " ++ show nDims ++ "\n" ++
    genArrayHeaders (nDims-1) offset

genArrayStorage :: Int -> Int -> String
genArrayStorage 0 offset = ""
genArrayStorage 1 offset = 
    "\tLOAD_R %fp\n" ++
    "\tLOAD_O " ++ show offset ++ "\n" ++
    "\tLOAD_O 1\n"
genArrayStorage nDims offset =
    (genArrayStorage (nDims-1) offset) ++
    "\tLOAD_R %fp\n" ++
    "\tLOAD_O " ++ show offset ++ "\n" ++
    "\tLOAD_O " ++ show nDims ++ "\n" ++
    "\tAPP MUL\n"



genIFbody :: Int -> I_fbody -> (Int,String)
genIFbody lNum (IFUN (label,fbodies,nVar,nArg,arrDims,stmts)) = (lNum3,
    label ++ ":" ++
    "\tLOAD_R %sp\n" ++
    "\tSTORE_R %fp\n" ++                      -- set new FP to top stack element
    "\tALLOC " ++ show nVar ++ "\n" ++        -- allocate nVar void cells
    "\tLOAD_I " ++ show (nVar+2) ++ "\n" ++   -- initializes deallocation counter, +2 for this counter and for removing code pointer at %fp 0

    arrayCode ++
    stmtCode ++                             -- value of return loaded

    "\tLOAD_R %fp\n" ++                       -- return from a function call
    "\tSTORE_O " ++ show (-(nArg+3)) ++ "\n" ++   -- write the return value to the first argument slot on the stack
    "\tLOAD_R %fp\n" ++                       -- write the return pointer to the second argument slot
    "\tLOAD_O 0\n" ++
    "\tLOAD_R %fp\n" ++
    "\tSTORE_O " ++ show (-(nArg+2)) ++ "\n" ++

    "\tLOAD_R %fp\n" ++                       -- deallocate local storage
    "\tLOAD_O " ++ show (nVar+1) ++ "\n" ++
    "\tAPP NEG\n" ++
    "\tALLOC_S\n" ++

    "\tSTORE_R %fp\n" ++                      -- restore the caller frame pointer
    "\tALLOC " ++ show (-nArg) ++ "\n" ++     -- clean up the arguments
    "\tJUMP_S\n\n" ++                           -- do jump to the return code pointer
    funCode 
    )
    where
        (lNum1,label1) = genLabel lNum
        arrayCode = (foldl (\acc arrDim -> acc ++ (genArray nVar arrDim)) "" arrDims)
        (lNum2,stmtCode) = foldWithLabel lNum1 genIStmt stmts
        (lNum3,funCode) = foldWithLabel lNum2 genIFbody fbodies

genIStmt :: Int -> I_stmt -> (Int,String) 
genIStmt lNum (IASS (level,offset,arrIndices,ie)) = (lNum,
    exprCode ++     -- push the expr value onto the stack
    arrCode ++      -- push the correct pointer and offset from fp onto the stack
    "\tSTORE_OS\n"
    ) where
        exprCode = genIExpr ie
        arrCode = genArrayAccess level offset arrIndices

genIStmt lNum (IWHILE (ie,is)) = (lNum3,
    label1 ++ ":" ++
    exprCode ++
    "\tJUMP_C " ++ label2 ++ "\n" ++
    stmtCode ++
    "\tJUMP " ++ label1 ++ "\n" ++
    label2 ++ ":"
    ) where
        exprCode = genIExpr ie
        (lNum1,label1) = genLabel lNum
        (lNum2,label2) = genLabel lNum1
        (lNum3,stmtCode) = genIStmt lNum2 is
        
genIStmt lNum (ICOND (ie,is1,is2)) = (lNum4,
    exprCode ++ 
    "\tJUMP_C " ++ label1 ++ "\n" ++  -- if false, jump to false label1
    stmtCode1 ++                    -- if true, do this
    "\tJUMP " ++ label2 ++ "\n" ++    -- jump to end
    label1 ++ ":" ++
    stmtCode2 ++
    label2 ++ ":"                 -- straight to end
    ) where
        exprCode = genIExpr ie
        (lNum1,label1) = genLabel lNum
        (lNum2,label2) = genLabel lNum1
        (lNum3,stmtCode1) = genIStmt lNum2 is1
        (lNum4,stmtCode2) = genIStmt lNum3 is2

genIStmt lNum (IREAD_F (level,offset,arrIndices)) = (lNum,
    "\tREAD_F\n" ++
    arrayCode ++
    "\tSTORE_OS\n"
    ) where 
        arrayCode = genArrayAccess level offset arrIndices

genIStmt lNum (IREAD_I (level,offset,arrIndices)) = (lNum,
    "\tREAD_I\n" ++
    arrayCode ++
    "\tSTORE_OS\n"
    ) where 
        arrayCode = genArrayAccess level offset arrIndices

genIStmt lNum (IREAD_B (level,offset,arrIndices)) = (lNum,
    "\tREAD_B\n" ++
    arrayCode ++
    "\tSTORE_OS\n"
    ) where 
        arrayCode = genArrayAccess level offset arrIndices

genIStmt lNum (IPRINT_F ie) = (lNum,
    exprCode ++ 
    "\tPRINT_F\n"
    ) where 
        exprCode = genIExpr ie

genIStmt lNum (IPRINT_I ie) = (lNum,
    exprCode ++ 
    "\tPRINT_I\n"
    ) where 
        exprCode = genIExpr ie

genIStmt lNum (IPRINT_B ie) = (lNum,
    exprCode ++ 
    "\tPRINT_B\n"
    ) where 
        exprCode = genIExpr ie

-- put expression on top of the stack
genIStmt lNum (IRETURN ie) = (lNum,exprCode) 
    where 
        exprCode = genIExpr ie

genIStmt lNum (IBLOCK (fbodies,nVar,arrDims,stmts)) = (lNum2,
    "\tLOAD_R %fp\n" ++                       -- save the old frame pointer as access link
    "\tALLOC 2\n" ++                          -- 2 void cells to be consistent with finding access link
    "\tLOAD_R %sp\n" ++
    "\tSTORE_R %fp\n" ++                      -- set new FP to top stack element
    "\tALLOC " ++ show nVar ++ "\n" ++        -- allocate nVar void cells
    
    "\tLOAD_I " ++ show (nVar+3) ++ "\n" ++   -- initializes deallocation counter, +2 to get rid of void cells

    arrayCode ++
    stmtCode ++                             

    "\tLOAD_R %fp\n" ++                       -- deallocate local storage
    "\tLOAD_O " ++ show (nVar+1) ++ "\n" ++
    "\tAPP NEG\n" ++
    "\tALLOC_S\n" ++                          -- now old frame pointer on top of stack

    "\tSTORE_R %fp\n\n" ++                    -- restore the caller frame pointer
    
    funCode 
    )
    where
        arrayCode = (foldl (\acc arrDim -> acc ++ (genArray nVar arrDim) ) "" arrDims)
        (lNum1,stmtCode) = foldWithLabel lNum genIStmt stmts
        (lNum2,funCode) = foldWithLabel lNum1 genIFbody fbodies           
        



-- push the correct sp and offset m onto the stack
genArrayAccess :: Int -> Int -> [I_expr] -> String
genArrayAccess level offset arrIndices = 
    case arrSlot of 
        Nothing -> 
                    jumpToLevel level ++                -- push the correct level fp onto the stack
                    "\tLOAD_I " ++ show offset ++ "\n"    -- push offset onto the stack
        Just accessCode -> 
                    jumpToLevel level ++                -- push the correct level fp onto the stack
                    "\tLOAD_O " ++ show offset ++ "\n" ++ -- top of stack has the pointer to array header
                    "\tLOAD_I " ++ show nDims ++ "\n" ++
                    accessCode ++
                    "\tAPP ADD\n"                         -- #dims (accounts for the header) + offset of slot
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
    "\tLOAD_O " ++ show offset ++ "\n" ++
    "\tLOAD_O " ++ show (nDims - length ais) ++ "\n" ++ 
    "\tLOAD_I 1\n" ++     
    "\tAPP SUB\n" ++      -- (d1-1)
    (genIExpr ai) ++
    "\tAPP MUL\n" ++      -- (d1-1) * r
    fromJust (genArraySlot level offset nDims ais) ++
    "\tAPP ADD\n"         -- (d1-1) * r + s



-- push value on top of the stack
genIExpr :: I_expr -> String
genIExpr (IINT n) = "\tLOAD_I " ++ show n ++ "\n"
genIExpr (IREAL n) = "\tLOAD_F " ++ show n ++ "\n"
genIExpr (IBOOL b) = "\tLOAD_B " ++ show b ++ "\n"
genIExpr (IID (level,offset,arrIndices)) = 
    arrayCode ++
    "\tLOAD_OS\n"
    where
        arrayCode = genArrayAccess level offset arrIndices
genIExpr (IAPP (op,ies)) =
    exprsCode ++
    opCode
    where
        exprsCode = foldl (\acc ie -> acc ++ (genIExpr ie)) "" ies
        opCode = genIOpn op

genIExpr (ISIZE (level,offset,dimNum)) = 
    jumpToLevel level ++
    "\tLOAD_O " ++ show offset ++ "\n" ++
    "\tLOAD_O " ++ show dimNum ++ "\n"



-- get the fp of the given level
jumpToLevel :: Int -> String 
jumpToLevel level = 
    "\tLOAD_R %fp\n" ++
    foldl (++) ""  (replicate level "\tLOAD_O -2\n")


genIOpn :: I_opn -> String
genIOpn (ICALL (label,level)) = 
    -- load args
    "\tALLOC 1\n" ++
    jumpToLevel level ++ 
    "\tLOAD_R %fp\n" ++
    "\tLOAD_R %cp\n" ++
    "\tJUMP " ++ label ++ "\n"
genIOpn IADD = "\tAPP ADD\n"
genIOpn IMUL = "\tAPP MUL\n"
genIOpn ISUB = "\tAPP SUB\n"
genIOpn IDIV = "\tAPP DIV\n"
genIOpn INEG = "\tAPP NEG\n"
genIOpn ILT = "\tAPP LT\n"
genIOpn ILE = "\tAPP LE\n"
genIOpn IGT = "\tAPP GT\n"
genIOpn IGE = "\tAPP GE\n"
genIOpn IEQ = "\tAPP EQ\n"    
genIOpn IADD_F = "\tAPP ADD_F\n"
genIOpn IMUL_F = "\tAPP MUL_F\n"
genIOpn ISUB_F = "\tAPP SUB_F\n"
genIOpn IDIV_F = "\tAPP DIV_F\n"
genIOpn INEG_F = "\tAPP NEG_F\n"
genIOpn ILT_F = "\tAPP LT_F\n"
genIOpn ILE_F = "\tAPP LE_F\n"
genIOpn IGT_F = "\tAPP GT_F\n"
genIOpn IGE_F = "\tAPP GE_F\n"
genIOpn IEQ_F = "\tAPP EQ_F\n"
genIOpn INOT = "\tAPP NOT\n"
genIOpn IAND = "\tAPP AND\n"
genIOpn IOR = "\tAPP OR\n"
genIOpn IFLOAT = "\tAPP FLOAT\n"
genIOpn ICEIL = "\tAPP CEIL\n"
genIOpn IFLOOR = "\tAPP FLOOR\n"
