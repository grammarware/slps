:- ensure_loaded('../slps.pro').
% wiki: ShowPNF

:- 
   % Compatibility hack for >6.4.1 and the use of '--'
   ( RawArgv = argv ; RawArgv = os_argv ),
   current_prolog_flag(RawArgv,Argv),
   append(_,['--',BtfFile],Argv),
   loadXml(BtfFile,Xml),
   xmlToRoot(Xml,R1),
   implodeRoot(R1,R2),
   writeq(R2),nl,
   halt.
