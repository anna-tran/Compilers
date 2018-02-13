# Compiler for Minisculus to basic stack machine code

This file is a compiler for the Minisculus language (as described below), written in Haskell using Alex, which produces basic stack machine code for a Minisculus program. The compiler works as follows
    1. Produce a list of tokens, each token associated with an AlexPosn and
       potentially with a value.
        * If there is an error with the syntax, then print out an error message with           the AlexPosn of where the error occured and end the program.
    2. Read the token list using the rules of the grammar.
        * If the tokens do not abide by the rules, then print out an error
          message and end the program.
    3. Print out the abstract syntax tree for the program.
    4. Produce the stack machine code and output it to a file named
       'machine_code'.

After sourcing the 'bashstack' file, sourcing the 'machine\_code' file will
execute the Minisculus program.

## Minisculus Grammar

prog -> stmt. 
stmt -> IF expr THEN stmt ELSE stmt
            | WHILE expr DO stmt
            | INPUT ID
            | ID ASSIGN expr
            | WRITE expr
            | BEGIN stmtlist END. 
stmtlist -> stmtlist stmt SEMICOLON
            |. 
expr -> expr addop term 
            | term. 
addop -> ADD
            | SUB. 
term -> term mulop factor 
            | factor. 
mulop -> MUL
            | DIV. 
factor -> LPAR expr RPAR
            | ID
            | NUM
            | SUB NUM.

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

Assure that Haskell and the Alex module, as well as the chs shell are installed.

To compile and use the program:
  1. Run
      alex lexer.x
  2. Run
      ghc -main-is Lexer -o lexer lexer.hs
  3. Run
      ./lexer \<input file\>
      where \<input file\> is the file with the contents you want to test
  4. Enter into the chs shell with the command 'csh'
  5. Run the ./bashstack script to setup the environment for the compiler
  6. To execute the program from the \<input file\>, run
      source machine_code
