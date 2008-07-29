Python (with pyparsing) implementation of FL with support for:

    - parsing
    - pretty printing
    - evaluation
    - optimization

All features (except parsing) are implemented as mixins for the base classes.

To test the parser run:

    $ python testparser.py program.fl output.fl

To evaluate an expression run:

    $ python testevaluator.py program.fl exprfile result

All testers return code 0 in case of success.

Note: PyFL requires Python 2.5 with pyparsing library on top!
