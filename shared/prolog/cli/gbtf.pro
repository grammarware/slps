:- ensure_loaded('../slps.pro').
:- ensure_loaded('../gbtf.pro').
:- nb_setval(btfno,1).
:- dynamic t/1.

saveT(BgfFile,N,Q,G1,T1)
 :-
    nb_getval(options,Options),
    require(checkbtf(r(G1,T1)),'BTF check failed',[]),
    ( member(strip,Options) ->
        ( stripT(T1,T2), G2 = g([],[]) );
        ( T2 = T1, G2 = G1 )
    ),
    ( t(T2) ->
        format('Skipping duplicate.~n',[]) ;
        (
          assertz(t(T2)),
          tToXml(r(G2,T2),XmlT),
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
    append(_,['--',BgfFile|Options],Argv),
    nb_setval(options,Options),
    loadXml(BgfFile, XmlG),
    xmlToG(XmlG,G),

% Compute grammar properties

    mindepthG(G),
    mindistG(G),
    definedNs(G,DNs),

% Output some statistics

    statistics(G),

% Generate shortest completion

    ( member(sc,Options) ->
        ( format('Generating shortest completion.~n',[]),
          (
            member(DN,DNs),
            completeG(G,DN,T0),
            saveT(BgfFile,DN,sc,G,T0),
            fail ; true
          )
        ) ; true ),

% Generate data for nonterminal coverage

    ( member(nc,Options) ->
        ( format('Generating data for nonterminal coverage.~n',[]),
          (
            member(DN,DNs),
            gbtf:mindistFact(DN,H,_),
            hostG(G,DN,H,T1,V),
            completeG(G,H,V),
            saveT(BgfFile,DN,nc,G,T1),
            fail ; true
          )
        ) ; true ),

% Generate data for context-depdendent coverage

    ( member(cdbc,Options) ->
        ( format('Generating data for context-depdendent coverage.~n',[]),
          (
            member(DN,DNs),
            (
              contextN(G,DN,P),
              varyG(G,P,T1)
            ;
              gbtf:mindistFact(DN,H,_),
              hostG(G,DN,H,T1,V),
              contextN(G,H,P),
              varyG(G,P,V)
            ),
            saveT(BgfFile,DN,cdbc,G,T1),
            fail ; true
          )
        ) ; true ),

% Done!

    halt.


% Run!

:- run.
