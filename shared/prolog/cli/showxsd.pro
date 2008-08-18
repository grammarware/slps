:- ensure_loaded('../slps.pro').

main 
 :- 
    current_prolog_flag(argv,Argv),
    append(_,['--',Input],Argv),
    loadXsd(Input,G1),
    completeXsd(G1,G2),
    ppBgf(G2),
    halt.

:- run.

