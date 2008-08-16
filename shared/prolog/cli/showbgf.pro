:- ensure_loaded('../slps.pro').

:- 
   current_prolog_flag(argv,Argv),
   append(_,['--',LgfFile],Argv),
   loadXml(LgfFile,Xml),
   xmlToG(Xml,G),
   ppBgf(G),
   halt.
