A Haskell-based implementation of FL that covers:
- parsing
- pretty-printing
- evaluation
- optimization

The implementation has been tested with GHC 6.6.1.
Parsing uses Text.ParserCombinators.ReadP.
The transformation for optimization relies on Data.Generics.
Pretty-printing maps terms to strings (using inefficient concatenation).

Run "make" to test the implementation.

