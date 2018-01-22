{
module Lexer where

import Data.List
import Data.Char
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$special = [\+\-\:\=\*\/\(\)\;\%]   -- other tokens


tokens :-
  $white+                   ; 
  "%" .*                    ;
  "/*"                      {\p s -> LCOMMENT}
  "*/"                      {\p s -> RCOMMENT}
  "if"                      {\p s -> IF p}
  "then"                    {\p s -> THEN p}
  "while"                   {\p s -> WHILE p}
  "do"                      {\p s -> DO p}
  "input"                   {\p s -> INPUT p} 
  "else"                    {\p s -> ELSE p}
  "begin"                   {\p s -> BEGIN p}
  "end"                     {\p s -> END p}
  "write"                   {\p s -> WRITE p}
  $alpha[$digit $alpha]*    {\p s -> ID p s}
  $digit+                   {\p s -> NUM p (read s)}
  \+                        {\p s -> ADD p}
  \:=                       {\p s -> ASSIGN p}
  \-                        {\p s -> SUB p}
  \*                        {\p s -> MUL p}
  \/                        {\p s -> DIV p}
  \(                        {\p s -> LPAR p}
  \)                        {\p s -> RPAR p}
  \;                        {\p s -> SEMICOLON p}  
  $special+                 {\p s -> ERROR p s}

  
{


--TODO function that deals with comments
comment :: [Token] -> Int -> [Token]
comment [] 0 = []
comment [] n
  | n > 0     = [ERROR (AlexPn 0 0 0) "Missing closing comment"]
  | n < 0     = [ERROR (AlexPn 0 0 0) "Missing opening comment"]
comment (x:xs) n
  | x == LCOMMENT   = comment xs (n+1)
  | x == RCOMMENT   = comment xs (n-1)
  | n < 0 || n > 0          = comment xs n
  | n == 0          = x:(comment xs n)

  
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
  | LCOMMENT
  | RCOMMENT
  | ERROR AlexPosn String
  deriving (Eq, Show)


lexer :: String -> IO [Token]
lexer file = do
  s <- readFile file
  --comment deals with the comment and error checking
  return (comment (alexScanTokens s) 0)

main = do
    s <- getContents
    print ("comment (alexScanTokens s) 0")
}