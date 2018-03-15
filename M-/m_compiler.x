{
module M_Compiler where

import Data.List
import Data.Char
import Data.Maybe
import Data.Either
import Data.Foldable

import System.Environment
import System.IO
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
  $white+                       ; 
  "%".*                         ;
  "/*"                          {\p s -> LCOMMENT p}
  "*/"                          {\p s -> RCOMMENT p}
  "if"                          {\p s -> IF p}
  "then"                        {\p s -> THEN p}
  "while"                       {\p s -> WHILE p}
  "do"                          {\p s -> DO p}
  "input"                       {\p s -> INPUT p} 
  "else"                        {\p s -> ELSE p}
  "begin"                       {\p s -> BEGIN p}
  "end"                         {\p s -> END p}
  "write"                       {\p s -> WRITE p}
  $alpha[$digit $alpha]*        {\p s -> ID p s}
  $digit+                       {\p s -> NUM p s}
  \+                            {\p s -> ADD p}
  \:=                           {\p s -> ASSIGN p}
  \-                            {\p s -> SUB p}
  \*                            {\p s -> MUL p}
  \/                            {\p s -> DIV p}
  \(                            {\p s -> LPAR p}
  \)                            {\p s -> RPAR p}
  \;                            {\p s -> SEMICOLON p}  
  .                             {\p s -> ERROR p ("Unknown symbol " ++ s ++ " at ")}

  
{

comment :: [Token] -> Int -> [Token]
comment [] 0 = []
comment (LCOMMENT p:ts) n 
  | ts == []              = [ERROR p "Missing closing comment at "] 
  | otherwise             = comment ts (n+1)
comment (RCOMMENT p:ts) n 
  | n <= 0  || ts == []   = [ERROR p "Unbalanced closing comment at "]
  | otherwise             = comment ts (n-1)
comment [t] n 
  | n > 0                 = [ERROR (position t) "Missing closing comment at "] 
comment (t:ts) n
  | n > 0                 = comment ts n
  | n == 0                = t:(comment ts n)

data Error = Error String

data Token
  = IF AlexPosn
  | THEN AlexPosn
  | WHILE AlexPosn
  | DO AlexPosn
  | INPUT AlexPosn
  | ELSE AlexPosn
  | BEGIN AlexPosn
  | END AlexPosn
  | WRITE AlexPosn
  | ADD AlexPosn
  | ASSIGN AlexPosn
  | SUB AlexPosn
  | MUL AlexPosn
  | DIV AlexPosn
  | LPAR AlexPosn
  | RPAR AlexPosn
  | SEMICOLON AlexPosn
  | LCOMMENT AlexPosn
  | RCOMMENT AlexPosn
  | ID AlexPosn String
  | NUM AlexPosn String
  | ERROR AlexPosn String
  deriving (Eq)

data Stmt = If Exp Stmt Stmt
          | While Exp Stmt 
          | Assign String Exp
          | Block [Stmt]
          | Print Exp
          | Input Exp
          --deriving (Show)

data Exp = Add Exp Exp   
         | Mul Exp Exp
         | Div Exp Exp 
         | Neg Exp
         | Id String
         | Num Integer  
         --deriving (Show)

stackStmt :: Int -> Stmt -> (String,Int)
stackStmt n (If e s1 s2) =  
    (expr
        ++ "cJUMP label" ++ (show n) ++ "\n"
        ++ code1
        ++ "JUMP label"++(show (n+1))++"\n"
        ++ "label"++(show n)++":\n"
        ++ code2
        ++ "label"++(show (n+1))++":\n"
        , m) where
    (expr) = stackExpr e
    (code1,n') = stackStmt (n+2) s1
    (code2,m) = stackStmt n' s2
stackStmt n (While e s) =
    ("label" ++ (show n) ++ ":\n"
        ++ expr
        ++ "cJUMP label" ++ (show (n+1)) ++ "\n"
        ++ code
        ++ "JUMP label" ++ (show n) ++ "\n"
        ++ "label" ++ (show (n+1)) ++ ":\n"
        , m) where
    (expr) = stackExpr e        
    (code,m) = stackStmt (n+2) s
stackStmt n (Assign s e) =
    (expr
        ++ "LOAD " ++ s ++ "\n"
    , n) where
    (expr) = stackExpr e
stackStmt n (Print e) =
    (expr
        ++ "PRINT\n"
    , n) where
    (expr) = stackExpr e
stackStmt n (Block (t:ts)) =
    (stmtCode
        ++ rmdCode
    , m) where
    (stmtCode,n') = stackStmt n t
    (rmdCode,m)  = stackStmt (n') (Block ts)
stackStmt n (Block []) = ("",n) 
stackStmt n (Input (Id s)) =                            
    ("READ " ++ s ++ "\n" , n)






stackExpr :: Exp -> String
stackExpr (Add e1 e2) =
    (code1
        ++ code2
        ++ "OP2 + \n"
    ) where
    code1 = stackExpr e1
    code2 = stackExpr e2
stackExpr (Mul e1 e2) = 
    (code1
        ++ code2
        ++ "OP2 * \n"
    ) where
    (code1) = stackExpr e1
    (code2) = stackExpr e2
stackExpr (Div e1 e2) = 
    (code1
        ++ code2
        ++ "OP2 / \n"
    ) where
    (code1) = stackExpr e1
    (code2) = stackExpr e2
stackExpr (Neg e) = 
    (code
        ++ "cPUSH -1 \n"
        ++ "OP2 * \n"
    ) where
    (code) = stackExpr e 
stackExpr (Id s) = ("rPUSH " ++ s ++ "\n")
stackExpr (Num i) = ("cPUSH " ++ (show i) ++ "\n")    






minParser :: [Token] -> Either String Stmt
minParser ((ERROR p s):ts) = Left (s ++ (show p))
minParser ts = 
    case (stmt ts) of 
        Left (Error msg) -> Left msg
        Right (s,rest) -> Right s

stmt:: [Token] -> Either Error (Stmt,[Token])
stmt ((IF _):rest) = 
    case (expr rest) of
        Left err -> Left err
        Right (e, rest1) ->
            case (thenpart rest1) of
                Left err -> Left err
                Right ((s1,s2),rest') -> Right (If e s1 s2,rest')


stmt ((WHILE _):rest) = 
    case (expr rest) of
        Left err -> Left err
        Right (e, rest1) ->
            case (dopart rest1) of
                Left err -> Left err
                Right (s,rest') -> Right (While e s,rest')


stmt ((INPUT p):rest) = 
    case (expr rest) of
        Left err -> Left err
        Right (e, rest') -> 
            case e of
                Num _ -> Left (Error $ "Error in input: input variable must start with an alphabetic character "++(show p))
                Id _ -> Right (Input e,rest')


stmt ((ID _ s):(ASSIGN _):rest) = 
    case (expr rest) of
        Left err -> Left err
        Right (e, rest') -> Right (Assign s e,rest')


stmt ((WRITE _):rest) = 
    case (expr rest) of
        Left err -> Left err
        Right (e, rest') -> Right (Print e,rest')


stmt ((BEGIN _):rest) = 
    case (stmtlist rest) of
        Left err -> Left err
        Right (sl, rest') -> Right (Block sl,rest')


stmt (t:ts) = Left (Error $ 
    "Error in stmt: expected a conditional statement, a loop, an input, an assignment, a write statement or a block of statements " 
    ++ (show (position t)))            

thenpart :: [Token] -> Either Error ((Stmt,Stmt),[Token])
thenpart ((THEN _):rest) = 
    case (stmt rest) of
        Left err -> Left err
        Right (s1, rest1) ->
            case (elsepart rest1) of
                Left err -> Left err
                Right (s2,rest') -> Right ((s1,s2),rest')

thenpart (t:ts) = Left (Error $ 
    "Error in Thenpart: expected keyword 'then' " 
    ++ (show (position t)))

elsepart :: [Token] -> Either Error (Stmt,[Token])            
elsepart ((ELSE _):rest) = (stmt rest)
elsepart (t:ts) = Left (Error $ 
    "Error in Elsepart: expected keyword 'else' " 
    ++ (show (position t)))

dopart :: [Token] -> Either Error (Stmt,[Token])            
dopart ((DO _):rest) = (stmt rest)
dopart (t:ts) = Left (Error $ 
    "Error in Dopart: expected keyword 'do' " 
    ++ (show (position t)))            



stmtlist :: [Token] -> Either Error ([Stmt],[Token])
stmtlist ts = (stmtlist1 ts)

      
stmtlist1 :: [Token] -> Either Error ([Stmt],[Token])
stmtlist1 [] = Left (Error "Error in Stmtlist: missing keyword 'end'")
stmtlist1 ((END _):rest) = Right ([],rest)
stmtlist1 ts = 
    case (stmt ts) of
        Left err -> Left err
        Right (s1, rest1) ->
            case (semicolonpart rest1) of
                Left err -> Left err
                Right (sl,rest') -> Right ((s1:sl),rest')


semicolonpart :: [Token] -> Either Error ([Stmt],[Token])
semicolonpart ((SEMICOLON _):rest) = (stmtlist1 rest)


semicolonpart (t:ts) = Left (Error $ 
    "Error in Semicolonpart: expected ; " 
    ++ (show (position t)))



expr :: [Token] -> Either Error (Exp,[Token])
expr ts = 
    case (term ts) of
        Left err -> Left err
        Right (e, ts1) ->
            case (expr1 ts1) of
                Left err -> Left err
                Right (f, ts') -> Right (f e,ts')



expr1:: [Token] -> Either Error (Exp -> Exp,[Token])    
expr1 ((ADD _):ts) = 
    case (term ts) of
        Left err -> Left err
        Right (e, ts1) ->
            case (expr1 ts1) of
                Left err -> Left err
                Right (f, ts') -> Right (g,ts')
                    where g x = f(Add x e)


expr1 ((SUB _):ts) = 
    case (term ts) of
        Left err -> Left err
        Right (e, ts1) ->
            case (expr1 ts1) of
                Left err -> Left err
                Right (f, ts') -> Right (g,ts')
                    where g x = f(Add x (Neg e))


expr1 ts = Right (g,ts)  where
            g x = x                         --  g::  Exp -> Exp    


term :: [Token] -> Either Error (Exp,[Token]) 
term ts = 
    case (factor ts) of
        Left err -> Left err
        Right (e, ts1) ->
            case (term1 ts1) of
                Left err -> Left err
                Right (f, ts') -> Right (f e,ts')



term1:: [Token] -> Either Error (Exp -> Exp,[Token])    
term1 ((MUL _):ts) = 
    case (factor ts) of
        Left err -> Left err
        Right (e', ts1) ->
            case (term1 ts1) of
                Left err -> Left err
                Right (f, ts') -> Right (g,ts')
                    where g x = f(Mul x e')


term1 ((DIV _):ts) =     
    case (factor ts) of
        Left err -> Left err
        Right (e', ts1) ->
            case (term1 ts1) of
                Left err -> Left err
                Right (f, ts') -> Right (g,ts')
                    where g x = f(Div x e')



term1 ts = Right (g,ts)  where
            g x = x                         --  g::  Exp -> Exp          

factor :: [Token] -> Either Error (Exp,[Token])
factor ((LPAR _):rest) = 
    case (expr rest) of
        Left err -> Left err
        Right (e, rest1) ->
            case (rparpart rest1) of
                Left err -> Left err
                Right rest' -> Right (e,rest')



factor ((ID _ s):rest) = Right (Id s,rest)
factor ((NUM _ s):rest) = Right (Num (read s),rest)
factor ((SUB _):rest) = 
    case (expr rest) of
        Left err -> Left err
        Right (e, rest') -> Right (Neg e,rest')

  

factor (t:ts) = Left (Error $ "Error in Factor: expected (, an identifier, or a number "++(show (position t)))

rparpart :: [Token] -> Either Error [Token]
rparpart ((RPAR _):rest) = Right rest
rparpart (t:ts) = Left (Error $ "Error in RParPart: expected ) " ++ (show ( position t)))

addTabs :: Int -> String
addTabs 0 = ""
addTabs n = "  " ++ (addTabs (n-1))

indent :: Int -> String -> String
indent _ [] = ""
indent 0 ts = ts
indent n (t:ts) 
    | t == '\n'     = t:((addTabs n) ++ (indent n ts))
    | otherwise     = t:((indent n ts))

concatStmts :: String -> [Stmt] -> String
concatStmts s [] = s
concatStmts s (x:xs) = (concatStmts s' xs) where
    s' = (s ++ (indent 1 ("\n" ++ (show x)) ))
    
instance Show Stmt where
    show (If e s1 s2) = 
        "If " ++ (show e)
        ++ (indent 1 (
            "\n" ++ "Then " ++ (show s1)
            ++ "\n" ++ "Else " ++ (show s2)))
    show (While e s) =
        "While " ++ (show e) ++ " Do"
        ++ (indent 1 ("\n" ++ (show s)))

    show (Assign s e) =
        "Assign " ++ s ++ " " ++ (show e)

    show (Print e) = 
        "Print " ++ (show e)
    show (Input e) = 
        "Input " ++ (show e)

    show (Block stmts) = 
        "Block [" 
        ++ (concatStmts "" stmts)
        ++ "\n]"
            


instance Show Exp where
    show (Add e1 e2)    = " (" ++ (show e1) ++ " + " ++ (show e2)++ ")"
    show (Mul e1 e2)    = " (" ++ (show e1) ++ " * " ++ (show e2)++ ")"
    show (Div e1 e2)    = " (" ++ (show e1) ++ " / " ++ (show e2)++ ")"
    show (Neg e)        = "Neg (" ++ (show e) ++ ")"
    show (Id s)         = "Id " ++ s
    show (Num i)        = "Num " ++ (show i)

   

instance Show Token where
    show (IF p) = "IF"
    show (THEN p) = "THEN"
    show (WHILE p) = "WHILE"
    show (DO p) = "DO"
    show (INPUT p) = "INPUT"
    show (ELSE p) = "ELSE"
    show (BEGIN p) = "BEGIN"
    show (END p) = "END"
    show (WRITE p) = "WRITE"
    show (ASSIGN p) = "ASSIGN"
    show (ADD p) = "ADD"
    show (SUB p) = "SUB"
    show (MUL p) = "MUL"
    show (DIV p) = "DIV"
    show (LPAR p) = "LPAR"
    show (RPAR p) = "RPAR"
    show (SEMICOLON p) = "SEMICOLON"
    show (ID p s) = "ID " ++ s
    show (NUM p s) = "NUM " ++ s
    show (ERROR p s) = "Error " ++ s ++ (show p)

position :: Token -> AlexPosn
position (IF p) = p
position (THEN p) = p
position (WHILE p) = p
position (DO p) = p
position (INPUT p) = p
position (ELSE p) = p
position (BEGIN p) = p
position (END p) = p
position (WRITE p) = p
position (ASSIGN p) = p
position (ADD p) = p
position (SUB p) = p
position (MUL p) = p
position (DIV p) = p
position (LPAR p) = p
position (RPAR p) = p
position (SEMICOLON p) = p
position (LCOMMENT p) = p
position (RCOMMENT p) = p
position (ID p _) = p
position (NUM p _) = p
position (ERROR p _) = p

getError :: [Token] -> Maybe [Token]
getError [] = Nothing
getError (err@(ERROR p s):xs) = Just [err]
getError (x:xs) = getError xs

fromEither :: Either String Stmt -> String
fromEither (Left s) = s
fromEither (Right s) = (show s)

getRight :: Either String Stmt -> Stmt
getRight (Right s) = s

getLeft :: Either String Stmt -> String
getLeft (Left s) = s

lexer :: String -> IO [Token]
lexer file = do
    s <- readFile file
    toks <- return (comment(alexScanTokens s) 0)
    maybeError <- return (getError toks)
    if (isNothing maybeError)
        then return toks
        else return (fromJust maybeError)


checkErrorToks :: [Token] -> Bool
checkErrorToks [] = False
checkErrorToks ((ERROR _ _):ts) = True
checkErrorToks ts = False

checkErrorStmt :: Either String Stmt -> Bool
checkErrorStmt (Left s) = True
checkErrorStmt (Right s) = False


  

main = do
    args <- getArgs
    let file = args !! 0
    toks <- (lexer file)
    if (checkErrorToks toks)
        then do
            print $ head toks
        else do
            let stmtls = minParser toks
            if (checkErrorStmt stmtls)
                then print (getLeft stmtls)
                else do
                    print (getRight stmtls)
                    let (str,counter) = (stackStmt 0 (getRight stmtls))
                    handle <- openFile "machine_code" WriteMode
                    hPutStrLn handle str
                    hClose handle
    

    
   
}
