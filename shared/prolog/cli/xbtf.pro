:- ensure_loaded('../slps.pro').
% wiki: XBTF

main 
 :- 
    % Compatibility hack for >6.4.1 and the use of '--'
    ( RawArgv = argv ; RawArgv = os_argv ),
    current_prolog_flag(RawArgv,Argv),
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
