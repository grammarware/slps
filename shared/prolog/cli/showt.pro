:- ensure_loaded('../slps.pro').

:- 
   current_prolog_flag(argv,Argv),
   append(_,['--',BtfFile],Argv),
   loadXml(BtfFile,Xml),
   xmlToRoot(Xml,T),
   ppT(T),
   halt.
