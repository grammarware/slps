:- ['Parser.pro'].
:- ['Optimizer.pro'].

main(File1,File2)
 :-
    parseFile(File1,expr,E1),
    parseFile(File2,expr,E2),
    optimize(E1,E2),
    optimize_variation(E1,E2).
