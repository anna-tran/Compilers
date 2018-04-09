# M+ Lexer, Parser and Semantic Analyzer

This program is a lexer, parser, semantic analyzer and code generator for the M+ language. It takes in an M+ program and on a
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
        * creates a .am file labelled "machine_code.am" containing the stack
          machine code for the given program
        
The stages of this program are
    1. Lexing
    2. Parsing
    3. Semantic Analysis
    4. Code Generation
Failure in one stage results in the program exiting and not moving forward with the next stages.
If any array indices are out of bounds, the program simply exits once it
reaches the line with out of bound indices.

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

# Running M+ programs

## Compilation

1. Run the makefile (with all .hs files in the current directory) using

	$ make


## Testing

There are sample M+ programs enclosed in this .zip file. Some will have errors in the program to show that semantic analysis works.
Testing must be done on the linux.cpsc.ucalgary.ca server.

To test a program, run

	$ ./Interpret [-s] <M+ program file>
    $ alias AM='/usr/bin/sml @SMLload=/home/411/AM/am+.x86-linux'
    $ AM [-d] machine_code.am

To disable verbose logging, use the option '-s'. This will omit the printing of the AST, linearized tree and intermediate representation of the program.
To run the machine code in debug mode, use the option '-d' in the third command.

# Plumbing Diagrams

The attached plumbing diagrams represent the logic of the program. While it is not exactly as the code is written, it depicts the general flow of the input 
and output data, as well as the interfaces for each component of the abstract syntax tree.
