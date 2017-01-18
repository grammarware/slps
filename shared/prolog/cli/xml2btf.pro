:- ensure_loaded('../slps.pro').
% wiki: XML2BTF

main 
 :-
    % Compatibility hack for >6.4.1 and the use of '--'
    ( RawArgv = argv ; RawArgv = os_argv ),
    current_prolog_flag(RawArgv,Argv),
    append(_,['--',XsdFile,XmlFile,BtfFile],Argv),
    loadXsd(XsdFile,SG),
    loadXml(XmlFile,XmlIn),
    rootToBtf(SG,XmlIn,Root),
    tToXml(Root,XmlOut),
    saveXml(BtfFile,XmlOut),
    halt.

:- run.
