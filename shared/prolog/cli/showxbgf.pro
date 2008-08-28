:- ensure_loaded('../slps.pro').

main :- 
   current_prolog_flag(argv,Argv),
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
