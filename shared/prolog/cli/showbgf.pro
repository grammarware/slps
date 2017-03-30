:- ensure_loaded('../slps.pro').
% wiki: ShowBGF

main :- 
   % Compatibility hack for >6.4.1 and the use of '--'
   ( RawArgv = argv ; RawArgv = os_argv ),
   current_prolog_flag(RawArgv,Argv),
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
