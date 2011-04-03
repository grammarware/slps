:- ensure_loaded('../slps.pro').
:- ensure_loaded('../gbtf.pro').
:- nb_setval(btfno,0).

saveT(BgfFile,XmlT)
 :-
    nb_getval(btfno,N1),
    N2 is N1 + 1,
    nb_setval(btfno,N2),
    concat_atom([BgfFile,'.',N1,'.btf'],BtfFile),
    saveXml(BtfFile,XmlT),
    !.

main 
 :- 
    current_prolog_flag(argv,Argv),
    append(_,['--',BgfFile],Argv),
    loadXml(BgfFile, XmlG),
    xmlToG(XmlG,G),
    ( mindepthG(G),
      findall(G1,contextG(G,G1),Gs), length(Gs,Len), write(Len), nl,
      shortestG(G,T),
      checkbtf(T),
      tToXml(T,XmlT),
      saveT(BgfFile,XmlT),
      fail
    ;
      nb_getval(btfno,N),
      require(N>0,'BTF generator failed',[]),
      halt
    ).

:- run.
