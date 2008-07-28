:- ensure_loaded('ll.pro').


main 
 :-
    current_prolog_flag(argv,Argv),
    append(_,['--',XsdFile,LgfFile],Argv),
    loadXsd(XsdFile,(_,G,_)),
    gToXml(G,Xml),
    saveXml(LgfFile,Xml),
    halt.

:- run.
