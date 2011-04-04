:- ensure_loaded('../slps.pro').
:- ensure_loaded('../gbtf.pro').
:- nb_setval(btfno,0).

saveT(BgfFile,T)
 :-
    checkbtf(T),
    tToXml(T,XmlT),
    nb_getval(btfno,N1),
    N2 is N1 + 1,
    nb_setval(btfno,N2),
    concat_atom([BgfFile,'.',N1,'.btf'],BtfFile),
    saveXml(BtfFile,XmlT),
    !.


statistics(G)
 :-
    rootG(G,R),
    format('* Root: ~w~n',[R]),

    findall(G1,contextG(G,G1),Gs),
    length(Gs,Contexts),
    format('* Contexts: ~w~n',[Contexts]),

    findall(H,gbtf:mindistFact(R,H,_),Hs),
    length(Hs,Holes),
    format('* Holes: ~w~n',[Holes]).


main 
 :- 
    current_prolog_flag(argv,Argv),
    append(_,['--',BgfFile],Argv),
    loadXml(BgfFile, XmlG),
    xmlToG(XmlG,G),

% Compute grammar properties

    mindepthG(G),
    mindistG(G),

% Output some statistics

    statistics(G),

% Generate shortest completion

    format('Generating shortest completion.~n',[]),
    completeG(G,T0),
    saveT(BgfFile,T0),

% Generate data for nonterminal coverage

    format('Generating data for nonterminal coverage.~n',[]),
    (
      rootG(G,R),
      gbtf:mindistFact(R,H,_),
      hostG(G,H,T1,V),
      completeN(G,H,V),
      saveT(BgfFile,T1),
      fail 
    ; 
      true
    ),

% Done!

    halt.

%    nb_getval(btfno,N),
%    require(N>0,'BTF generator failed',[]),

:- run.
