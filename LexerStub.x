{
module Lexer where

import Data.List
import Data.Char
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]
$special = [\+\-\*\/\(\)\;\%]   -- other tokens


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
  .                             {\p s -> ERROR p s}

  
{


--TODO function that deals with comments
comment :: [Token] -> Int -> [Token]
comment [] 0 = []
comment [RCOMMENT p] n  = [ERROR p "Missing closing comment"]
  where n > 1
comment (x:xs) n
  | x == LCOMMENT p      = comment xs (n+1)
  | x == RCOMMENT p      = comment xs (n-1)
  | n > 0               = comment xs n
  | n < 0               = 
  | n == 0              = x:(comment xs n)

  
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
  | ID AlexPosn String
  | NUM AlexPosn String
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
  | ERROR AlexPosn String
  deriving (Eq, Show)


lexer :: String -> IO [Token]
lexer file = do
  s <- getContents
  -- s <- readFile file
  -- comment deals with the comment and error checking
  return (comment (alexScanTokens s) 0)

main = do
   -- s <- getContents
   toks <- lexer "t1.txt"
   print toks
}