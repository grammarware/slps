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
    G = g(_,Ps),

    ( allNs(Ps,Ns), 
      length(Ns,Len),
      format('* All nonterminals: ~w~n',[Len]),
      fail; true ),

    ( definedNs(Ps,Ns), 
      length(Ns,Len),
      format('* Defined nonterminals: ~w~n',[Len]),
      fail; true ),

    ( topNs(Ps,Ns),
      length(Ns,Len),
      format('* Top nonterminals: ~w~n',[Len]),
      fail; true ),

    ( startsymbol(G,R),
      format('* Start symbol: ~w~n',[R]),
      fail; true ),

    ( length(Ps,Len), 
      format('* Productions: ~w~n',[Len]),
      fail; true ),

    ( findall(P2,(member(P1,Ps),contextG(P1,P2)),PsWithContext),
      length(PsWithContext,Len),
      format('* Contexts: ~w~n',[Len]),
      fail; true ),

    ( findall((N1,N2),gbtf:mindistFact(N1,N2,_),N12s),
      length(N12s,Len),
      format('* Holes: ~w~n',[Len]),
      fail; true ).


% Best effort to determine start symbol
 
startsymbol(G,R)
 :-
    (
      G = g([R],_)
    ;
      topNs(G,[R])
    ; 
      definedNs(G,DNs),
      member(R,DNs),
      ssWeight(R,W1),
      \+ ( member(N,DNs), \+ N == R, ssWeight(N,W2), W2 >= W1 )
    ),
    !.

ssWeight(N,W)
 :-
    findall(Q,gbtf:mindistFact(N,Q,_),Qs),
    length(Qs,W).


% Backtrack over root nonterminals

rootG(G,R)
 :-
    nb_getval(options,Options),
    member(root,Options) -> 
      require(startsymbol(G,R),'start symbol not defined/inferrable',[]) ;
      ( definedNs(G,DNs), member(R,DNs) ).


%
% Optimization: skip upper "casies"?
%

skipuppy(p(_,N,_)) 
 :-
    skipuppy(N).

skipuppy(N)
 :-
    atom(N),
    nb_getval(options,Options),
    member(uppy,Options),
    upcase_atom(N,N).    


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

% Output some statistics

    statistics(G),

% Generate shortest completion

    ( member(sc,Options) ->
        ( format('Generating shortest completion.~n',[]),
          (
            rootG(G,R),
            completeT(G,n(R),T),
            saveT(BgfFile,R,sc,G,T),
            fail ; true
          )
        ) ; true ),

% Generate data for nonterminal coverage

    ( member(nc,Options) ->
        ( format('Generating data for nonterminal coverage.~n',[]),
          (
            rootG(G,R),
            gbtf:mindistFact(R,H,_),
            holeT(G,n(R),H,T,V),
            completeT(G,n(H),V),
            saveT(BgfFile,R,nc,G,T),
            fail ; true
          )
        ) ; true ),

% Generate data for rule coverage

    ( member(rc,Options) ->
        ( format('Generating data for rule coverage.~n',[]),
          (
            rootG(G,R),
            (
              findN(G,R,Ps),
              member(P,Ps),
              completeT(G,P,T)
            ;
              gbtf:mindistFact(R,H,_),
              \+ skipuppy(H),
              findN(G,H,Ps),
              holeT(G,n(R),H,T,V),
              member(P,Ps),
              completeT(G,P,V)
            ),
            saveT(BgfFile,R,rc,G,T),
            fail ; true
          )
        ) ; true ),

% Generate data for branch coverage

    ( member(bc,Options) ->
        ( format('Generating data for branch coverage.~n',[]),
          (
            rootG(G,R),
            (
              findN(G,R,Ps),
              member(P,Ps),
              forkG(P,F),
              varyT(G,F,T)
            ;
              gbtf:mindistFact(R,H,_),
              \+ skipuppy(H),
              holeT(G,n(R),H,T,V),
              findN(G,H,Ps),
              member(P,Ps),
              forkG(P,F),
              varyT(G,F,V)
            ),
            saveT(BgfFile,R,bc,G,T),
            fail ; true
          )
        ) ; true ),

% Generate data for context-depdendent branch coverage

    ( member(cdbc,Options) ->
        ( format('Generating data for context-dependent branch coverage.~n',[]),
          (
            rootG(G,R),
            (
              findN(G,R,Ps),
              member(P,Ps),
              contextG(P,F),
              varyT(G,F,T)
            ;
              gbtf:mindistFact(R,H,_),
              \+ skipuppy(H),
              holeT(G,n(R),H,T,V),
              findN(G,H,Ps),
              member(P,Ps),
              contextG(P,F),
              varyT(G,F,V)
            ),
            saveT(BgfFile,R,cdbc,G,T),
            fail ; true
          )
        ) ; true ),

% Done!

    halt.


% Run!

:- run.
