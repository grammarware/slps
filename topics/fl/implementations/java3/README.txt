A Java-based implementation of FL that covers:
- unmarshaling XML data into objects
- pretty-printing
- evaluation

The implementation requires Java 6 and AspectJ 1.6.
Java 6 is required for the sake of JAXB support.
AspectJ is used for intertype declarations.
The object model has been derived with the xjc schema compiler.
Pretty printing and evaluation are implemented as as intertype declarations.
Pretty printing uses inefficient string concatenation.

Run "make" to test the implementation.
