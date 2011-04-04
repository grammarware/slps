:- ensure_loaded('../slps.pro').
:- ensure_loaded('../gbtf.pro').
:- nb_setval(btfno,1).
:- dynamic t/1.

saveT(BgfFile,N,Q,T)
 :-
    require(checkbtf(T),'BTF check failed',[]),
    ( t(T) ->
        format('Skipping duplicate.~n',[]) ;
        (
          assertz(t(T)),
          tToXml(T,XmlT),
          nb_getval(btfno,I1),
          I2 is I1 + 1,
          nb_setval(btfno,I2),
          concat_atom([BgfFile,'-',N,'-',Q,'-',I1,'.btf'],BtfFile),
          saveXml(BtfFile,XmlT)
        )
    ).


statistics(G)
 :-

    findall(P,(definedNs(G,DNs),member(DN,DNs),contextN(G,DN,P)),Ps),
    length(Ps,Contexts),
    format('* Contexts: ~w~n',[Contexts]),

    findall((N1,N2),gbtf:mindistFact(N1,N2,_),N12s),
    length(N12s,Holes),
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
    definedNs(G,DNs),

% Output some statistics

    statistics(G),

% Generate shortest completion

    format('Generating shortest completion.~n',[]),
    (
      member(DN,DNs),
      completeG(G,DN,T0),
      saveT(BgfFile,DN,sc,r(G,T0)),
      fail
    ;
      true
    ),

% Generate data for nonterminal coverage

    format('Generating data for nonterminal coverage.~n',[]),
    (
      member(DN,DNs),
      gbtf:mindistFact(DN,H,_),
      hostG(G,DN,H,T1,V),
      completeG(G,H,V),
      saveT(BgfFile,DN,nc,r(G,T1)),
      fail 
    ; 
      true
    ),

% Generate data for context-depdendent coverage

    format('Generating data for context-depdendent coverage.~n',[]),
    (
      member(DN,DNs),
      (
        contextN(G,DN,P),
        varyG(G,P,T1)
      ;
        gbtf:mindistFact(DN,H,_),
        hostG(G,DN,H,T1,V),
        contextN(G,H,P),
        varyN(G,P,V)
      ),
      saveT(BgfFile,DN,cdbc,r(G,T1)),
      fail
    ; 
      true
    ),

% Done!

    halt.


% Run!

:- run.
