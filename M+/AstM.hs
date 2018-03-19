
module AstM where

data M_prog = M_prog ([M_decl],[M_stmt])

data M_decl = M_var (String,[M_expr],M_type)
            | M_fun (String,[(String,Int,M_type)],M_type,[M_decl],[M_stmt])

data M_stmt = M_ass (String,[M_expr],M_expr)
            | M_while (M_expr,M_stmt)
            | M_cond (M_expr,M_stmt,M_stmt) 
            | M_read (String,[M_expr])
            | M_print M_expr
            | M_return M_expr
            | M_block ([M_decl],[M_stmt])

data M_type = M_int | M_bool | M_real 
            deriving (Eq, Ord, Show, Read)

data M_expr = M_ival Integer
            | M_rval Float
            | M_bval Bool
            | M_size (String,Int)
            | M_id (String,[M_expr])
            | M_app (M_operation,[M_expr])     

data M_operation = M_fn String | M_add | M_mul | M_sub | M_div | M_neg
                 | M_lt | M_le | M_gt | M_ge | M_eq | M_not | M_and | M_or
                 | M_float | M_floor | M_ceil
                deriving (Eq, Ord, Show, Read)        
                
                      

instance Show M_prog where
    show (M_prog (decls, stmts)) = indent $ "M_prog \n(" ++ (showMyList decls) ++ ",\n" ++ (showMyList stmts) ++ ")"

instance Show M_decl where
    show (M_var (str, exprs, type_))      = "M_var (" ++ (encloseQuotes str) ++ "," ++ (showMyList exprs) ++ "," ++ (show type_) ++ ")"
    show (M_fun (str, args, type_, decls, stmts))
                                        = indent $ "M_fun \n(" ++ (encloseQuotes str) ++ "," ++ (showListStrings (funArgsToString args)) ++ "," ++ (show type_)
                                            ++ "\n," ++ (showMyList decls) ++ "\n," ++ (showMyList stmts) ++ ")"

instance Show M_stmt where
    show (M_ass (str, exprs, expr))       = "M_ass (" ++ (encloseQuotes str) ++ "," ++ (showMyList exprs) ++ "," ++ (show expr) ++ ")"
    show (M_while (expr, stmt))           = "M_while (" ++ (show expr) ++ indent ( ",\n" ++ (show stmt) ++ ")")
    show (M_cond (expr, stmt1, stmt2))    = "M_cond (" ++ (show expr) ++ indent (",\n" ++ (show stmt1) ++ ",\n" ++ (show stmt2) ++ ")")
    show (M_read (str, exprs))            = "M_read (" ++ (encloseQuotes str) ++ "," ++ (showMyList exprs) ++ ")"
    show (M_print expr)                   = "M_print (" ++ (show expr) ++ ")"
    show (M_return expr)                  = "M_return (" ++ (show expr) ++ ")"
    show (M_block (decls, stmts))         = indent $ "M_block (\n" ++ (showMyList decls) ++ ",\n" ++ (showMyList stmts) ++ ")"


instance Show M_expr where
    show (M_ival integer)         = "M_ival " ++ (show integer)
    show (M_rval real)            = "M_rval " ++ (show real)
    show (M_bval bool)            = "M_bval " ++ (show bool)
    show (M_size (str,integer))   = "M_size (" ++ (encloseQuotes str) ++ "," ++ (show integer) ++ ")"
    show (M_id (str, exprs))      = "M_id (" ++ (encloseQuotes str) ++ "," ++ (showMyList exprs) ++ ")"
    show (M_app (op, exprs))      = indent $ "M_app (" ++ (show op) ++ ",\n" ++ (showMyList exprs) ++ ")"

funArgsToString :: [(String,Int,M_type)] -> [String]
funArgsToString [] = []
funArgsToString (x@(str,i,t):xs) = ( "(" ++ (encloseQuotes str) ++ "," ++ (show i) ++ "," ++ (show t) ++ ")" ) : (funArgsToString xs)

showListStringsRaw :: [String] -> String
showListStringsRaw [] = ""
showListStringsRaw [a] = a
showListStringsRaw (x:xs) = x ++ "," ++ (showListStringsRaw xs)

showListStrings :: [String] -> String
showListStrings [] = ""
showListStrings strs = "[" ++ (showListStringsRaw strs) ++ "]"     

showMyListRaw :: (Show a) => [a] -> String
showMyListRaw [] = ""
showMyListRaw [a] = (show a)
showMyListRaw (x:xs) = (show x) ++ ",\n" ++ (showMyList xs)

showMyList :: (Show a) => [a] -> String
-- showMyList [a] = showMyListRaw [a]
showMyList arr = "[" ++ (showMyListRaw arr) ++ "]"

encloseQuotes :: String -> String
encloseQuotes s = "\"" ++ s ++ "\""


addTabs :: Int -> String
addTabs 0 = ""
addTabs n = "    " ++ (addTabs (n-1))

indent :: String -> String
indent [] = ""
indent (t:ts) 
    | t == '\n'     = t:((addTabs 1) ++ (indent ts))
    | otherwise     = t:((indent ts))


