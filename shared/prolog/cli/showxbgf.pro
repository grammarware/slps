:- ensure_loaded('../slps.pro').
% wiki: ShowXBGF

main :- 
   % Compatibility hack for >6.4.1 and the use of '--'
   ( RawArgv = argv ; RawArgv = os_argv ),
   current_prolog_flag(RawArgv,Argv),
   append(_,['--',XbgfXsdFile,XbgfXbgfFile,XbgfFile],Argv),
   loadXsd(XbgfXsdFile,SG),
   loadXml(XbgfXbgfFile,XbgfXbgfXml),
   loadXml(XbgfFile,XbgfXml),
   rootToBtf(SG,XbgfXml,RootIn),
   xml2xbgf(XbgfXbgfXml,Xbgf),
   transformT(Xbgf,RootIn,RootOut),
   implodeRoot(RootOut,Imploded),
   ppList('  ', Imploded),nl,
   halt.

:- run.
