:- ensure_loaded('../slps.pro').


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
   require(
     subset(Ps2,Ps1),
     'Used grammar must be subset of declared grammar.',
     []),
   halt.

:- run.
