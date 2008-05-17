:- ['Parser.pro'].
:- ['PrettyPrinter.pro'].
:- ['Evaluator.pro'].
:- ['Optimizer.pro'].

main(File1,File2)
 :-
    parseFile(File1,Fs),
    ppToFile(File2,Fs),
    evaluate(Fs,apply(fac,[literal(5)]),120),
    ToBeOptimized =
	      ifThenElse(
		binary(equal,literal(0),literal(1)),
		literal(42),
		binary(plus,literal(43),literal(45))),
    optimize(ToBeOptimized,literal(88)).
