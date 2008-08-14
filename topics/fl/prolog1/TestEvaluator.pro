:- ['Parser.pro'].
:- ['Evaluator.pro'].

main(File1,File2,R)
 :-
    parseFile(File1,program,Fs),
    parseFile(File2,expr,E),
    evaluate(Fs,E,R).
