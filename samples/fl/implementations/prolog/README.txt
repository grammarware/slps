A Prolog-based implementation of FL that covers:
- parsing
- pretty-printing
- evaluation
- optimization

The implementation has been tested with SWI-Prolog 5.6.50.
Parsing relies on a scannerless style of DCGs.
The transformation for optimization relies on a made-up traversal library.
Pretty-printing maps terms to strings (using inefficient concatenation).

Run "make" to test the implementation.

Here is a literature reference on Prological Language Processing.
This reference refers to seminal literature in the field.

@inproceedings{LR01,
 author = "Ralf L{\"a}mmel and G{\"u}nter Riedewald",
 editor = "Brand, {M.G.J. van den} and D.~Parigot",
 title = "{Prological Language Processing}",
 month = apr,
 year = 2001,
 booktitle = "{Proceedings of the First Workshop on Language Descriptions, Tools
 and Applications (LDTA 2001)}",
 publisher = "Elsevier Science",
 series = "ENTCS",
 volume = "44",
 issue = "2",
}
