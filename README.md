# Lexer

This file is a lexical analyzer, written in Haskell, which produces a list of tokens parsed
using the following grammar. Each token is associated with an AlexPosn, and
potentially with a value. In a valid list of tokens, only the token name and
potentially its value, are given. If there is a an error with the syntax, then the
analyzer will just report a token list of one error, with an error message and the AlexPosn of where that error occurs.

## Tokens

The tokens stand for the following lexemes:
"if" => IF 
"then" => THEN 
"while" => WHILE
"do" => DO 
"input" => INPUT
"else" => ELSE 
"begin" => BEGIN 
"end" => END 
"write" => WRITE
{alpha}[{digit}{alpha}]\* => ID (identifier) 
{digit}+ => NUM (positive integer) 
"+" => ADD 
":=" => ASSIGN
"-" => SUB 
"\*" => MUL
"/" => DIV
"(" => LPAR
")" => RPAR
";"=> SEMICOLON

## How to use

Assure that Haskell and the Alex module are installed.

To compile and use the program:
  1. Run
      alex lexer.x
  2. Run
      ghc -main-is Lexer -o lexer lexer.hs
  3. Run
      ./lexer \<input file\>
      where \<input file\> is the file with the contents you want to test

