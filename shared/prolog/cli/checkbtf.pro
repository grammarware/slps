:- ensure_loaded('../slps.pro').
% wiki: CheckBTF


main :-
   % Compatibility hack for >6.4.1 and the use of '--'
   ( RawArgv = argv ; RawArgv = os_argv ),
   current_prolog_flag(RawArgv,Argv),
   append(_,['--',BtfFileIn],Argv),
   loadXml(BtfFileIn, BtfXml),
   xmlToRoot(BtfXml,Btf),
   ( checkbtf(Btf) -> halt(0); halt(1) ).

:- run.
