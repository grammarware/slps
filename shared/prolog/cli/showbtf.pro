:- ensure_loaded('../slps.pro').

:- 
   current_prolog_flag(argv,Argv),
   append(_,['--',BtfFile],Argv),
   loadXml(BtfFile,Xml),
   xmlToRoot(Xml,R1),
   implodeRoot(R1,R2),
   writeq(R2),nl,
   halt.
