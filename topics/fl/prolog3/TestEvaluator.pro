:- ['../../../shared/prolog/slps.pro'].
:- ['../prolog1/Evaluator.pro'].

int2literal(I1,literal(I3))
 :-
    atom_chars(I1,I2),
    number_chars(I3,I2).

main
 :-
    current_prolog_flag(argv,Argv),
    append(_,['--',Result1,BtfIn,FName|Ints],Argv),
    atom_chars(Result1,Result2),
    number_chars(Result3,Result2),
    load_structure(BtfIn, [Funs1], [dialect(xmlns)]),
    xmlToRoot(Funs1,Funs2),
    implodeRoot(Funs2,Funs3),
    maplist(int2literal,Ints,Args),
    evaluate(Funs3,apply(FName,Args),Result3).
