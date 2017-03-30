:- ensure_loaded('../slps.pro').
% wiki: XBGF

main 
 :- 
    % Compatibility hack for >6.4.1 and the use of '--'
    ( RawArgv = argv ; RawArgv = os_argv ),
    current_prolog_flag(RawArgv,Argv),
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
