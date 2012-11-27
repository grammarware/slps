A Java-based implementation of FL that covers:
- parsing
- pretty-printing
- evaluation

The implementation has been tested with Java 5 and 6.

The parser relies on ANTLR2 (has been tested with 1.1.7 and 1.2.2)
That jar needs to be in the CLASSPATH!

Pretty printing and evaluation are implemented as visitors.
Pretty printing uses inefficient string concatenation.

Run "make" to test the implementation.
