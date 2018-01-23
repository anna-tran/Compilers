{
module Lexer where

import Data.List
import Data.Char
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
  .                             {\p s -> ERROR p ("Unkown symbol "++s)}

  
{


comment :: [Token] -> Int -> [Token]
comment [] 0 = []
comment (LCOMMENT p:ts) n = comment ts (n+1)
comment (RCOMMENT p:ts) n 
  | n <= 0                = [ERROR p "Missing opening comment"]
  | otherwise             = comment ts (n-1)
comment [t] n | n > 0     = [ERROR (position t) "Missing closing comment"] 
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
  deriving (Eq, Show)

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


lexer :: String -> IO [Token]
lexer file = do
  s <- readFile file
  -- comment deals with the comment and error checking
  return (comment (alexScanTokens s) 0)

main = do
   args <- getArgs
   let file = args !! 0
   toks <- lexer file
   print toks
}
