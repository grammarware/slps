:- ensure_loaded('../slps.pro').
% wiki: ShowXSD

main 
 :- 
    current_prolog_flag(argv,Argv),
    append(_,['--',Input],Argv),
    loadXsd(Input,G1),
    completeXsd(G1,G2),
    ppG(G2),
    halt.

:- run.

