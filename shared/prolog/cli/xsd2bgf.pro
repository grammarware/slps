:- ensure_loaded('../slps.pro').

main 
 :-
    current_prolog_flag(argv,Argv),
    append(_,['--',XsdFile,BgfFile],Argv),
    loadXsd(XsdFile,(_,G,_)),
    gToXml(G,Xml),
    saveXml(BgfFile,Xml),
    halt.

:- run.
