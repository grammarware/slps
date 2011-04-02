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
    ( generateT(G,T),
      checkbtf(T),
      tToXml(T,XmlT),
      saveT(BgfFile,XmlT),
      fail
    ;
      nb_getval(btfno,N),
      ( N > 0 -> halt;
          ( write('gbtf failed.'),
            nl,
            halt(1)
          ) 
      )
    ).

:- run.
