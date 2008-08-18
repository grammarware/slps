:- ensure_loaded('../slps.pro').

main 
 :-
    current_prolog_flag(argv,Argv),
    append(_,['--',XsdFile,XmlFile,BtfFile],Argv),
    loadXsd(XsdFile,SG),
    loadXml(XmlFile,XmlIn),
    rootToBtf(SG,XmlIn,Root),
    tToXml(Root,XmlOut),
    saveXml(BtfFile,XmlOut),
    halt.

:- run.
