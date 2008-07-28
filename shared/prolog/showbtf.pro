:- ensure_loaded('ll.pro').


:- 
   current_prolog_flag(argv,Argv),
   append(_,['--',LtrFile],Argv),
   loadXml(LtrFile,Xml),
   xmlToLtr(Xml,T),
   ppT(T),
   halt.
