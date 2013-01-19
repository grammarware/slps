:- ensure_loaded('../slps.pro').
% wiki: XSD2BGF

main 
 :-
    current_prolog_flag(argv,Argv),
    append(_,['--',XsdFile,BgfFile],Argv),
    loadXsd(XsdFile,G1),
    completeXsd(G1,G2),
    gToXml(G2,Xml),
    saveXml(BgfFile,Xml),
    halt.

:- run.
