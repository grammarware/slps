A Haskell-based implementation of FL that covers:
- parsing
- pretty-printing
- evaluation
- optimization

The implementation has been tested with GHC 6.6.1.
Parsing uses module Text.ParserCombinators.ReadP.
The transformation for optimization relies on module Data.Generics.
Both modules are softly wrapped to provide more convenience. 
Pretty-printing maps terms to strings (using inefficient concatenation).

Run "make" to test the implementation.
