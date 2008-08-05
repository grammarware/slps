:- ensure_loaded('slps.pro').


main 
 :- 
    current_prolog_flag(argv,Argv),
    append(_,['--',Input],Argv),
    load_structure(Input, Xsd, [dialect(xmlns)]),
    member(S,Xsd),
    S =.. [element|_],
    gFromSchema(S,G1),
    normalizeG(G1,G2),
    ppG(G2),
    halt.

:- run.

