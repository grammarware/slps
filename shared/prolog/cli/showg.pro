:- ensure_loaded('../slps.pro').
% wiki: ShowG

:- 
   % Compatibility hack for >6.4.1 and the use of '--'
   ( RawArgv = argv ; RawArgv = os_argv ),
   current_prolog_flag(RawArgv,Argv),
   append(_,['--',LgfFile],Argv),
   loadXml(LgfFile,Xml),
   xmlToG(Xml,G),
   ppG(G),
   halt.
