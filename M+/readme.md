# M+ Lexer and Parser

This program is a lexer and parser for the M+ language. It takes in an M+ program and on a
1. Failed lex or parse
	* prints out a "Parse failed" message
	* prints out the list of tokens it was able to lex
	* prints out the error message

2. Successful lex and parse
	* prints out a "Parse successful" message
	* prints out the abstract syntax tree for the given M+ program

## Note
* Allow arrays to be accessed by any indice at compile time because some array
  dimension sizes may not be known until runtime.
* Allow variables of the same name to be declared in different scopes.

# Compilation

1. Run the makefile (with all .hs files in the current directory) using

	make


# Testing

There are 5 sample M+ programs enclosed in this .zip file. They are all valid programs and will not throw syntax errors.

To test a program, run

	./Interpret [-s] <M+ program file>

To disable verbose logging, use the option '-s'. Otherwise, the linearized tree of the M+ program file will also be printed out.	
