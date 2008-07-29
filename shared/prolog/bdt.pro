:- ensure_loaded('slps.pro').


% Keep track of difference

error(1).


% Compare grammars

diffG(RC,((U1,G1),(U2,G2)))
 :-
    format('Diffing ~w and ~w.~n',[U1,U2]),

    % Compare defined nonterminals
 
    definedNs(G1,DNs1),
    definedNs(G2,DNs2),
    subtract(DNs1,DNs2,DNsOnly1),
    subtract(DNs2,DNs1,DNsOnly2),
    intersection(DNs1,DNs2,DNsCommon),
    ( 
      (
        DNsOnly1 == [],
        DNsOnly2 == []
      ) -> 
          format(' - Names of defined nonterminals agree.~n',[])
        ; (
            error(RC),
            format(' - Names of defined nonterminals differ.~n',[]),
            format('   - Intersection ~w.~n',[DNsCommon]),
            format('   - ~w only: ~w.~n',[U1,DNsOnly1]),
            format('   - ~w only: ~w.~n',[U2,DNsOnly2])
          )
    ),

    % Compare definitions structurally

    format(' - Comparisons per (common) nonterminal:~n',[]),
    maplist(diffN(RC,G1,G2),DNsCommon),

    % Compare roots

    rootNs(G1,Rs1),
    rootNs(G2,Rs2),
    (
      (
        subset(Rs1,Rs2),
        subset(Rs2,Rs1) 
      ) ->
          format(' - Roots agree.~n',[])
        ; (
            error(RC),
            format(' - Roots differ: ~w vs. ~w.~n',[Rs1,Rs2])
          )
    ),

    !.


% Compare definitions structurally

diffN(RC,g(_,Ps1),g(_,Ps2),N)
 :-
    splitN(Ps1,N,PsN1,_,_),
    splitN(Ps2,N,PsN2,_,_),
    maplist(arg(1),PsN1,Ls1),
    maplist(arg(1),PsN2,Ls2),
    maplist(arg(3),PsN1,Xs1),
    maplist(arg(3),PsN2,Xs2),
    zip(Ls1,Xs1,LXs1),
    zip(Ls2,Xs2,LXs2),
    length(LXs1,L1),
    length(LXs2,L2),
    diffLXs(LXs1,LXs2,Q1),
    diffLXs(LXs2,LXs1,Q2),
    ( ( L1 == L2, Q1 == [], Q2 == [] ) ->
          format('   - Ok: ~w.~n',[N])
        ; (
            error(RC),
            format('   - Fail: ~w.~n',[N]),
            maplist(format('      - ~w~n'),Q1),
            format('     vs.~n',[]),
            maplist(format('      - ~w~n'),Q2)
          )
    ),
    !.
      

% We do not use subset/2 here because we care about doubles.

diffLXs(LXs1,LXs2,Q) :- diffLXs(LXs1,LXs2,[],Q).
diffLXs([],_,Q,Q).
diffLXs([LX1|LXs1],LXs2,Q1,Q3)
 :-
    ( (    
        append(LXs2a,[LX2|LXs2b],LXs2),
        diffEq(LX1,LX2)
      ) ->
          ( 
            append(LXs2a,LXs2b,LXs3),
            Q2 = Q1
          )
        ; ( 
            LXs3 = LXs2,
            Q2 = [LX1|Q1]
          )
    ),   
    diffLXs(LXs1,LXs3,Q2,Q3),
    !.


% Liberal equality test

diffEq(LX,LX) :- !.
diffEq((As,';'(Xs1)),(As,';'(Xs2)))
 :-
    length(Xs1,Len),
    length(Xs2,Len),
    subset(Xs1,Xs2),
    subset(Xs2,Xs1),
    !.


% Load a BGF file

load_bgf(Uri,G3)
 :-
    load_structure(Uri, [G1], [dialect(xmlns)]),
    format('Normalizing ~w.~n',[Uri]),
    xmlToG(G1,G2),
    normalizeG(G2,G3),
    !.

:- 
   current_prolog_flag(argv,Argv),
   append(_,['--'|L1],Argv),
   maplist(load_bgf,L1,Gs),
   zip(L1,Gs,L2),
   findall((G1,G2),(append(_,[G1|L3],L2),append(_,[G2|_],L3)),L4),
   maplist(diffG(RC),L4),
   ( RC = 0; true ),
   halt(RC).
