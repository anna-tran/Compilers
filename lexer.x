{
module Lexer where

import Data.List
import Data.Char
import Data.Maybe
import System.Environment
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
  .                             {\p s -> ERROR p ("Unkown symbol " ++ s ++ " at")}

  
{


comment :: [Token] -> Int -> [Token]
comment [] 0 = []
comment (LCOMMENT p:ts) n 
  | ts == []              = [ERROR p "Missing closing comment at"] 
  | otherwise             = comment ts (n+1)
comment (RCOMMENT p:ts) n 
  | n <= 0                = [ERROR p "Missing opening comment at"]
  | otherwise             = comment ts (n-1)
comment [t] n | n > 0     = [ERROR (position t) "Missing closing comment at"] 
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

lexer :: String -> IO [Token]
lexer file = do
  s <- readFile file
  toks <- return (comment(alexScanTokens s) 0)
  maybeError <- return (getError toks)
  if (isNothing maybeError)
    then return toks
    else return (fromJust maybeError)
  

main = do
  args <- getArgs
  let file = args !! 0
  toks <- (lexer file)
  print toks
}
