:- ensure_loaded('../slps.pro').
% wiki: ShowXSD

main 
 :- 
    % Compatibility hack for >6.4.1 and the use of '--'
    ( RawArgv = argv ; RawArgv = os_argv ),
    current_prolog_flag(RawArgv,Argv),
    append(_,['--',Input],Argv),
    loadXsd(Input,G1),
    completeXsd(G1,G2),
    ppG(G2),
    halt.

:- run.

