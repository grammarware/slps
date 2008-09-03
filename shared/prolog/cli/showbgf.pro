:- ensure_loaded('../slps.pro').

main :- 
   current_prolog_flag(argv,Argv),
   append(_,['--',BgfXsdFile,BgfXbgfFile,BgfFile],Argv),
   loadXsd(BgfXsdFile,SG),
   loadXml(BgfXbgfFile,BgfXbgfXml),
   loadXml(BgfFile,BgfXml),
   rootToBtf(SG,BgfXml,RootIn),
   xml2xbgf(BgfXbgfXml,Xbgf),
   transformT(Xbgf,RootIn,RootOut),
   implodeRoot(RootOut,Imploded),
   ppG(Imploded),nl,
   halt.

:- run.
