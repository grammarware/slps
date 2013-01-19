:- ensure_loaded('../slps.pro').
% wiki: XBGF

main 
 :- 
    current_prolog_flag(argv,Argv),
    append(_,['--',XbgfFile,BgfInFile,BgfOutFile],Argv),
    ( exists_file(BgfOutFile) -> delete_file(BgfOutFile); true ),
    loadXml(XbgfFile, XbgfXml),
    xml2xbgf(XbgfXml,Xbgf),
    loadXml(BgfInFile, BgfInXml),
    xmlToG(BgfInXml,G1),
    transformG(Xbgf,G1,G2),
    gToXml(G2,BgfOutXml),
    saveXml(BgfOutFile,BgfOutXml),
    halt.

:- run.
