:- ensure_loaded('../slps.pro').
% wiki: XSD2BGF

main 
 :-
    % Compatibility hack for >6.4.1 and the use of '--'
    ( RawArgv = argv ; RawArgv = os_argv ),
    current_prolog_flag(RawArgv,Argv),
    append(_,['--',XsdFile,BgfFile],Argv),
    loadXsd(XsdFile,G1),
    completeXsd(G1,G2),
    gToXml(G2,Xml),
    saveXml(BgfFile,Xml),
    halt.

:- run.
