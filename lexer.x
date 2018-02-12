{
module Lexer where

import Data.List
import Data.Char
import Data.Maybe
import Data.Either
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
  .                             {\p s -> ERROR p ("Unkown symbol " ++ s ++ " at ")}

  
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
--        deriving (Show)
data Exp = Add Exp Exp   
         | Mul Exp Exp
         | Div Exp Exp 
         | Neg Exp
         | Id String
         | Num Integer  
--        deriving (Show)

stackStmt :: Int -> Stmt -> (String,Int)
stackStmt n (If e s1 s2) =  
    (expr 
        ++ "cJUMP label" ++ (show n) ++ "\n"
        ++ code2
        ++ "JUMP label"++(show (n+1))++"\n"
        ++ "label"++(show n)++":\n"
        ++ code1
        ++ "label"++(show (n+1))++":\n"
        , m) where
    (expr) = stackExpr e
    (code1,n') = stackStmt (n+2) s1
    (code2,m) = stackStmt n' s2
stackStmt n (While e s) =
    ("label" ++ (show n) ++ ":\n"
        ++ (show e)
        ++ "cJUMP label" ++ (show (n+1)) ++ "\n"
        ++ "JUMP label" ++ (show (n+2)) ++ "\n"
        ++ "label" ++ (show (n+1)) ++ ":\n"
        ++ code
        ++ "JUMP label" ++ (show n) ++ "\n"
        ++ "label" ++ (show (n+2))
        , m) where
    (code,m) = stackStmt (n+3) s
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
    (stmtCode,n') = stackStmt (n+1) t
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
stackExpr (Id s) = ("rPush " ++ s ++ "\n")
stackExpr (Num i) = ("cPush " ++ (show i) ++ "\n")    






minParser :: [Token] -> Either String Stmt
minParser ((ERROR p s):ts) = Left (s ++ (show p))
minParser ts = (Right s) where
        (s, rest) = stmt ts

stmt:: [Token] -> (Stmt,[Token])
stmt ((IF _):rest) = (If e s1 s2,rest') where
            (e,rest1) = expr rest
            ((s1,s2),rest') = thenpart rest1
stmt ((WHILE _):rest) = (While e s,rest') where
            (e,rest1) = expr rest
            (s,rest') = dopart rest1
stmt ((INPUT _):rest) = (Input e,rest') where
            (e,rest') = expr rest
stmt ((ID _ s):(ASSIGN _):rest) = (Assign s e,rest') where
            (e,rest') = expr rest
stmt ((WRITE _):rest) = (Print e,rest') where
            (e,rest') = expr rest
stmt ((BEGIN _):rest) = (Block sl,rest') where
            (sl,rest') = stmtlist rest

thenpart :: [Token] -> ((Stmt,Stmt),[Token])
thenpart ((THEN _):rest) = ((s1,s2),rest') where
            (s1,rest1) = stmt rest
            (s2,rest') = elsepart rest1

elsepart :: [Token] -> (Stmt,[Token])            
elsepart ((ELSE _):rest) = (s,rest') where
            (s,rest') = stmt rest

dopart :: [Token] -> (Stmt,[Token])            
dopart ((DO _):rest) = (s,rest') where
            (s,rest') = stmt rest


stmtlist :: [Token] -> ([Stmt],[Token])
stmtlist ts = (sl,ts') where
      (sl,ts') = stmtlist1 ts
      
stmtlist1 :: [Token] -> ([Stmt],[Token])
stmtlist1 ((END _):rest) = ([],rest)
stmtlist1 ts = ((s1:sl),rest') where
            (s1,rest1) = stmt ts
            (sl,rest') = semicolonpart rest1

semicolonpart :: [Token] -> ([Stmt],[Token])
semicolonpart ((SEMICOLON _):rest) = (sl,rest') where
      (sl,rest') = stmtlist1 rest



expr :: [Token] -> (Exp,[Token])
expr ts = (f e,ts') where
      (e,ts1) = term ts
      (f,ts') = expr1 ts1

expr1:: [Token] -> (Exp -> Exp,[Token])    
expr1 ((ADD _):ts) = (g,ts')  where 
            (e', ts1) = term ts
            (f, ts') = expr1 ts1
            g x = f(Add x e')               --  g::  Exp -> Exp
expr1 ((SUB _):ts) = (g,ts')  where 
            (e', ts1) = term ts
            (f, ts') = expr1 ts1
            g x = f(Add x (Neg e'))         --  g::  Exp -> Exp   
expr1 ts = (g,ts)  where
            g x = x                         --  g::  Exp -> Exp    


term :: [Token] -> (Exp,[Token]) 
term ts = (f e,ts') where
            (e,ts1) = factor ts
            (f,ts') = term1 ts1

term1:: [Token] -> (Exp -> Exp,[Token])    
term1 ((MUL _):ts) = (g,ts')  where 
            (e', ts1) = factor ts
            (f, ts') = term1 ts1
            g x = f(Mul x e')               --  g::  Exp -> Exp
term1 ((DIV _):ts) = (g,ts')  where 
            (e', ts1) = factor ts
            (f, ts') = term1 ts1
            g x = f(Div x e')               --  g::  Exp -> Exp         
term1 ts = (g,ts)  where
            g x = x                         --  g::  Exp -> Exp          

factor :: [Token] -> (Exp,[Token])
factor ((LPAR _):rest) = (e,rest') where
            (e,rest1) = expr rest
            (rest') = rparpart rest1
factor ((ID _ s):rest) = (Id s,rest)
factor ((NUM _ s):rest) = (Num (read s),rest)
factor ((SUB _):rest) = (Neg e,rest') where
            (e,rest') = expr rest         

rparpart :: [Token] -> [Token]
rparpart ((RPAR _):rest) = rest




instance Show Stmt where
    show s = str 
        where
        (str,counter) = stackStmt 0 s

instance Show Exp where
    show e = str 
        where
        (str) = stackExpr e       

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
    show (ID p s) = "ID " ++ show s
    show (NUM p s) = "NUM " ++ show s
    show (ERROR p s) = "ERROR " ++ show s ++ " " ++ show p

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

lexer :: String -> IO [Token]
lexer file = do
    s <- readFile file
    toks <- return (comment(alexScanTokens s) 0)
    maybeError <- return (getError toks)
    if (isNothing maybeError)
        then return toks
        else return (fromJust maybeError)

writeToFile :: Handle -> [String] -> IO ()
writeToFile h [] = do

writeToFile h [t:ts] = do
    hPutStrLn h t
    writeToFile h ts
    
  

main = do
    args <- getArgs
    let file = args !! 0
    toks <- (lexer file)
    let stmtls = minParser toks
    handle <- openFile "machine_code" WriteMode

    writeToFile handle $ lines (fromEither stmtls)
    hClose handle
   
}
