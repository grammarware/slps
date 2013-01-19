:- ensure_loaded('../slps.pro').
% wiki: XML2BTF

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
