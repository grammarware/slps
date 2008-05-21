A Smalltalk based implementation of FL that covers:

  - parsing
  - pretty-printing
  - evaluation

The factorial language is implemented in "Factorial-Language.cs". To study the implementation you can directly browse the file within your favorite text editor. However to give it a try for real, you need to load it into the Smalltalk environment as described in the section below.

Have fun!

Lukas Renggli, <http://www.lukas-renggli.ch>
Software Composition Group, University of Bern
May 21, 2008


Installation
------------

The implementation has been tested with Squeak 3.9 and 3.10. To get started download Squeak for your platform from <http://www.squeak.org>.

The implementation depends on several libraries that have to be loaded into the development environment. Drag the file "Load-Dependencies.cs" into the running Squeak window and select "install into new change set" from the menu showing up. Do the same with the file "Factorial-Language.cs".


Documentation
-------------

The class FLFactorialParser defines the scanner and parser for the FL programming language using the parser combinator framework PetitParser.

The subclass FLFactorialCompiler defines productions to create a Smalltalk AST from the FL source. The Smalltalk AST can be trivially transformed to Smalltalk bytecodes and executed using the infrastructure of the development environment.

The subclass FLFactorialPrinter implements the pretty printer of the FL language. 

PPFactorialExample implements the code given in "factorial.txt". The code can be edited directly in the Smalltalk code browser and is automatically parsed, transformed and compiled down to Smalltalk bytecodes.

The examples can be executed by printing the result of evaluating the following expressions. Select the line and press Ctrl + P or Apple + P depending on your platform:

  FLFactorialExample new fac: 6.

  FLFactorialExample new fib: 10.
  
  FLFactorialExample new ack: 2 with: 3.