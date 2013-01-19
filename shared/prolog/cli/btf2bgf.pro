:- ensure_loaded('../slps.pro').
% wiki: BTF2BGF


main :-
   current_prolog_flag(argv,Argv),
   append(_,['--',BtfFileIn,BgfFileOut],Argv),
   loadXml(BtfFileIn, BtfXml),
   xmlToRoot(BtfXml,Btf),
   Btf = r(G1,T),
   btf2bgf(Btf,G2),
   gToXml(G2,BgfXml),   
   saveXml(BgfFileOut,BgfXml),
   ( subsetG(G2,G1) -> true; (
       write('Error: used grammar is not contained in declared grammar.'),
       nl,
       RC = 1
   )),
   ( checkbtf(G1,T) -> true; (
       write('Error: structural integrity of tree violated.'),
       nl,
       RC = 1
   )),
   ( RC = 0; true ),
   halt(RC).

:- run.
