module TransIR where

import SymbolTypes
import SymbolTable
import AstM
import IR
import Data.Maybe


transAstIR :: M_prog -> I_prog
transAstIR (M_prog (decls,stmts)) = do
		runState -1
		ip <- createEmptyIprog
		est <- empty 
		st <- newScope L_PROG est
		(st1,ip1) <- insertDecls st ip decls
		(st2,ip2) <- insertStmts st1 ip1 decls stmts


createEmptyIprog :: I_prog
createEmptyIprog = IPROG ([],0,[],[])

insertDecls :: ST -> I_prog -> [M_decl] -> (ST,I_prog)
insertDecls st ip [] = (st,ip)
insertDecls st ip (d:ds) = insertDecls st1 ip1 ds
	where
		(st1,ip1) = insertDecl st ip d

-- decl must not exist in symtable
-- insert into symtable, insert into iprog
insertDecl :: ST -> I_prog -> M_decl -> (ST,I_prog)
insertDecl st ip (M_var (id,dims,mtype)) =
	let pvar = lookUp st id in case of 
		Just x -> error $ id " is already defined as a variable"
		Nothing -> 
			where
				st1 = insert st (VARIABLE (id, mtype, length dims))
				I_VARIABLE(lvl,offset,t,ds) = fromJust $ lookUp st1 id
				ip1 = insertIRDecl st ir 

transIExpr :: M_expr -> I_expr