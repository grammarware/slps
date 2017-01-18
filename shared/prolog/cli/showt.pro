:- ensure_loaded('../slps.pro').
% wiki: ShowT

:- 
   % Compatibility hack for >6.4.1 and the use of '--'
   ( RawArgv = argv ; RawArgv = os_argv ),
   current_prolog_flag(RawArgv,Argv),
   append(_,['--',BtfFile],Argv),
   loadXml(BtfFile,Xml),
   xmlToRoot(Xml,T),
   ppT(T),
   halt.
