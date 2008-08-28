:- ensure_loaded('../slps.pro').

main 
 :- 
    current_prolog_flag(argv,Argv),
    append(_,['--',XbgfFile,BtfInFile,BtfOutFile],Argv),
    ( exists_file(BtfOutFile) -> delete_file(BtfOutFile); true ),
    loadXml(XbgfFile, XbgfXml),
    loadXml(BtfInFile, BtfInXml),
    xml2xbgf(XbgfXml,Xbgf),
    xmlToRoot(BtfInXml,RootIn),
    transformT(Xbgf,RootIn,RootOut),
    tToXml(RootOut,BtfOutXml),
    saveXml(BtfOutFile,BtfOutXml),
    halt.

:- run.
