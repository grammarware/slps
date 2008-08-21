:- ensure_loaded('../slps.pro').


% Keep track of difference

error(1).


main :-
   current_prolog_flag(argv,Argv),
   append(_,['--',BtfFileIn,BgfFileOut],Argv),
   loadXml(BtfFileIn, BtfXml),
   xmlToRoot(BtfXml,Btf),
   Btf = r(G1,T),
   btf2bgf(T,G2),
   gToXml(G2,BgfXml),   
   saveXml(BgfFileOut,BgfXml),
   G1 = g(_,Ps1),
   G2 = g(_,Ps2),
   ( subset(Ps2,Ps1) -> true; (
       write('Error: used grammar is not contained in declared grammar.'),
       nl,
       error(RC)
   )),
   ( checkbtf(Ps1,T) -> true; (
       write('Error: structural integrity of tree violated.'),
       nl,
       error(RC)
   )),
   ( RC = 0; true ),
   halt(RC).

:- run.
