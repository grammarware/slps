:- ensure_loaded('../slps.pro').

:- 
   current_prolog_flag(argv,Argv),
   append(_,['--',XbgfXsdFile,XbgfXbgfFile,XbgfFile],Argv),
   loadXsd(XbgfXsdFile,SG),
   loadXml(XbgfXbgfFile,XbgfXbgfXml),
   loadXml(XbgfFile,XbgfXml),
   rootToBtf(SG,XbgfXml,RootIn),
   xml2xbgf(XbgfXbgfXml,Xbgf),
   transformR(Xbgf,RootIn,RootOut),
   implodeRoot(RootOut,Imploded),
   writeq(Imploded),nl,
   halt.
