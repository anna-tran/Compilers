# M+ Lexer, Parser and Semantic Analyzer

This program is a lexer, parser and semantic analyzer for the M+ language. It takes in an M+ program and on a
1. Failure with
    1.1 Lex or Parse
    	* prints out a "Parse failed" message
	    * prints out the list of tokens it was able to lex
    	* prints out the error message
    1.2 Semantic Analysis
        * prints out a "Semantic Analysis failed" message
        * prints out the first error encountered while semantic checking


2. Success
    2.1 Lex or Parse
    	* prints out a "Parse successful" message
	    * prints out the abstract syntax tree for the given M+ program
    2.2 Semantic Analysis
        * prints out a "Semantic Analysis successful" message
        * prints out the Intermediate Representation of the program
        
The stages of this program are
    1. Lexing
    2. Parsing
    3. Semantic Checking
Failure in one stage results in the program exiting and not moving forward with
the next stages.

## Assumptions
* Allow arrays to be accessed by any index at compile time because some array dimension sizes may not be known until runtime.
* Allow variables of the same name to be declared in different scopes.
* Accessing arrays must be done using the exact number of dimensions
  specified for the array. For example, with
    var a[2][3]:int;
    var b[3]:int;
  we cannot assign
    a[0] := b;
  because only one dimension, as opposed to two, have been indicated.
* The 'float' operation can only be applied to integers.
* The 'ceil' and 'floor' operation can only be applied to real numbers.
* Values of one type can only be operated on with values of the same type.

# Compilation

1. Run the makefile (with all .hs files in the current directory) using

	make


# Testing

There are sample M+ programs enclosed in this .zip file. Some will have errors in the program to show that semantic analysis works.

To test a program, run

	./Interpret [-s] <M+ program file>

To disable verbose logging, use the option '-s'. Otherwise, the linearized tree of the M+ program file will also be printed out.	
