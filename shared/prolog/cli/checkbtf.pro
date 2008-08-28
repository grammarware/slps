:- ensure_loaded('../slps.pro').


main :-
   current_prolog_flag(argv,Argv),
   append(_,['--',BtfFileIn],Argv),
   loadXml(BtfFileIn, BtfXml),
   xmlToRoot(BtfXml,Btf),
   ( checkbtf(Btf) -> halt(0); halt(1) ).

:- run.
