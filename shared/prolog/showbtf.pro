:- ensure_loaded('slps.pro').


:- 
   current_prolog_flag(argv,Argv),
   append(_,['--',BtfFile],Argv),
   loadXml(BtfFile,Xml),
   xmlToBtf(Xml,T1),
   implodeT(T1,T2),
   writeq(T2),nl,
   halt.
