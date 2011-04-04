%
% Static namespace declarations
%

:- multifile sxmlns/2.

sxmlns(bgf,'http://planet-sl.org/bgf').


% Return all root nonterminals of a grammar

rootNs(g(Rs1,_),Rs2)
 :-
    list_to_set(Rs1,Rs2),
    !.


% Best-effort predication of start symbol

rootG(g(Rs,Ps),R)
 :-
    Rs = [R|_] ->
      true;
      Ps = [p(_,R,_)|_].

rootG(Ps,R)
 :-
    is_list(Ps),
    Ps = [p(_,R,_)|_].


% Return all nontmerinals defined by a grammar

definedNs(g(_,Ps),Ns)
 :-
    definedNs(Ps,Ns).


% Return all nontmerinals defined by a list of productions

definedNs(Ps,Ns2)
 :-
    ( Ps = []; Ps = [_|_] ),
    maplist(definedN,Ps,Ns1),
    list_to_set(Ns1,Ns2),
    !.


definedN(p(_,N,_),N).


% Return all nontmerinals used by a grammar or a fragment thereof

usedNs(G,Ns2)
 :-
    collect(usedN,G,Ns1),
    list_to_set(Ns1,Ns2),
    !.

usedN(n(N),[N]).


% Return all nonterminals defined or used by a grammar

allNs(G,Ns5)
 :-
    rootNs(G,Ns1),
    definedNs(G,Ns2),
    usedNs(G,Ns3),
    union(Ns1,Ns2,Ns4),
    union(Ns4,Ns3,Ns5),
    !.

allNs(Z,Ns3)
 :-
    \+ Z = g(_,_), 
    definedNs(Z,Ns1),
    usedNs(Z,Ns2),
    union(Ns1,Ns2,Ns3),
    !.

% Return all terminals

allTs(G,Ts2)
 :-
    collect(find_terminal,G,Ts1),
    list_to_set(Ts1,Ts2),
    !.

find_terminal(t(T),[T]).


% Return all labels

allLs(Z,Ls1)
 :-
    collect(find_label,Z,Ls1),
    !.

find_label(l(L),[L]).


% Return all selectors

allSs(G,Ss2)
 :-
    collect(find_selector,G,Ss1),
    list_to_set(Ss1,Ss2),
    !.

find_selector(s(S,_),[S]).


% Find a single production by LHS and possibly label

findP(Ps1,P1,Ps1a,Ps1b)
 :-
    require(
      (
        append(Ps1a,[P2|Ps1b],Ps1),
        eqP(P1,P2)
      ),
      'Production ~q not found.',
      [P1]).

findP(Ps1,As,N,P,Ps3a,Ps4a)
 :-
    splitN(Ps1,N,Ps2,Ps3,Ps4),
    append(Ps2a,[P|Ps2b],Ps2),
    P = p(As,N,_),
    require(
      (
        \+ member(p(As,N,_),Ps2a),
        \+ member(p(As,N,_),Ps2b)
      ),
      'Production must be addressed unambiguously by ~q.',
      [(As,N)]),
    append(Ps3,Ps2a,Ps3a),
    append(Ps2b,Ps4,Ps4a),
    !.


% Find definitions of nonterminal

findN(Ps1,N,Ps2)
 :- 
    splitN(Ps1,N,Ps2,_,_).

findN1(Ps,N,P)
 :- 
    splitN1(Ps,N,P,_,_).


% Split productions into those for N and their pre- and postfix

splitN(g(_,Ps1),N,Ps,Ps2,Ps3)
 :-
    !,
    splitN(Ps1,N,Ps,Ps2,Ps3).

splitN(Ps1,N,Ps,Ps2,Ps3)
 :-
    require(
      member(p(_,N,_),Ps1),
      'Nonterminal ~q must be defined.',
      [N]),
    append(Ps2,PsRest,Ps1),
    \+ member(p(_,N,_),Ps2),
    PsRest = [p(_,N,_)|_],
    !,
    filter(unifiable(p(_,N,_)),PsRest,Ps),
    filter(nonunifiable(p(_,N,_)),PsRest,Ps3),
    !.

splitN1(Z,N,P,Ps2,Ps3)
 :-
    splitN(Z,N,Ps,Ps2,Ps3),
    require(
      Ps = [P],
      'Nonterminal ~q must be defined by a single production.',
      [N]
    ),
    !.


% Split productions into the one for L and its pre- and postfix

splitL(Ps1,L,P1,Ps2a,Ps2b)
 :-
    require(
      (
        P1 = p(As1,_,_),
        append(Ps2a,[P1|Ps2b],Ps1),
        member(l(L),As1)
      ),
      'Label ~q not found.',
      [L]),
    require(
      ( \+ ( 
          append(Ps2a,Ps2b,Ps3),
          P2 = p(As2,_,_),
          member(P2,Ps3),
          member(l(L),As2)
        )   
      ),
      'Label ~q must be unambiguous.',
      [L]),
    !.


% Remove marker from grammar fragment

unmarkG(X,Y) :- transform(unmark_helper,X,Y).
unmark_helper({X},X) :- !.
unmark_helper(X,X).


% Normalization

normalizeG(Z1,Z4)
 :-
    normalizeG_grouping(Z1,Z2),
    normalizeG_algebraically(Z2,Z3),
    !,
    Z3 = Z4.


% All productions for a nonterminal are held contiguously.

normalizeG_grouping(g(Rs,Ps1),g(Rs,Ps2))
 :-
    !,
    normalizePs_grouping(Ps1,Ps2).

% Succeed if not applied to an entire grammar
normalizeG_grouping(Z,Z).


normalizePs_grouping([],[]).
normalizePs_grouping([P|Ps1],Ps6)
 :-
    P = p(_,N,_),
    filter(unifiable(p(_,N,_)),Ps1,Ps2),
    filter(nonunifiable(p(_,N,_)),Ps1,Ps3),
    normalizePs_grouping(Ps3,Ps4),
    remove_doubles([P|Ps2],Ps5),
    append(Ps5,Ps4,Ps6),
    !.

remove_doubles(Ps1,Ps2) 
 :-
    list_to_set(Ps1,Ps2).


%
% Normalizations:
% - Eliminate trivial sequences and choices.
% - Realize units of sequences and choices.
%

normalizeG_algebraically(Z1,Z2)
 :-
    ytransform(normalize_algebraically_rules,Z1,Z2),
    !.

normalize_algebraically_rules(','([]),true).
normalize_algebraically_rules(','([X]),X).
normalize_algebraically_rules(';'([]),fail).
normalize_algebraically_rules(';'([X]),X).
normalize_algebraically_rules('+'(true),true).
normalize_algebraically_rules('*'(true),true).
normalize_algebraically_rules('?'(true),true).

normalize_algebraically_rules(','(Xs1),','(Xs5))
 :-
    append(Xs2,[','(Xs3)|Xs4],Xs1),
    concat([Xs2,Xs3,Xs4],Xs5).

normalize_algebraically_rules(','(Xs1),','(Xs2))
 :-
    append(Xs1a,[true|Xs1b],Xs1),
    append(Xs1a,Xs1b,Xs2).

normalize_algebraically_rules(';'(Xs1),';'(Xs5))
 :-
    append(Xs2,[';'(Xs3)|Xs4],Xs1),
    concat([Xs2,Xs3,Xs4],Xs5).

normalize_algebraically_rules(';'(Xs1),';'(Xs2))
 :-
    append(Xs1a,[fail|Xs1b],Xs1),
    append(Xs1a,Xs1b,Xs2).

normalize_algebraically_rules(';'(Xs1),';'(Xs2))
 :-
    append(Xs1a,[X|Xs1b],Xs1),
    append(Xs1a,Xs1b,Xs2),
    append(_,[X|_],Xs2).

% Pretty print grammar

ppG(g(Rs,Ps))
 :-
    format('g( ~q, ',[Rs]),
    ppList('  ',Ps),
    format(')~n',[]),
    !.


% Test prefix property of grammar

prefixG(g(_,Ps))
 :-
     % Labels must be used unambiguously.
     maplist(arg(1),Ps,Lss),
     concat(Lss,Ls),
     list_to_set(Ls,Ls),

     % Horizontal choices are not permitted.
     rectest(not(=(';'(_))),Ps),

     % Nonterminals with multiple productions must be labelled. 
     findall(N,label_prop(Ps,N),Ns1),
     list_to_set(Ns1,Ns2),
     soft(
       ( Ns2 == [] ),
       'Definition with insufficient labeling: ~q.',
       [Ns2]).

label_prop(Ps,N)
 :-
    member(P1,Ps),
    member(P2,Ps),
    \+ P1 == P2,
    P1  = p([],N,_),
    P2  = p(_,N,_).

% Test grammars for subset relationship

subsetG(g(_,Ps1),Ps2)
 :-
    !,
    subsetG(Ps1,Ps2).

subsetG(Ps1,g(_,Ps2))
 :-
    !,
    subsetG(Ps1,Ps2).

subsetG(Ps1,Ps2)
 :-
    subset(Ps1,Ps2).

% Compare grammars

diffG(((U1,G1),(U2,G2)))
 :-
    diffG(RC,((U1,G1),(U2,G2))),
    !,
    RC = 0.


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
            RC = 1,
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
            RC = 1,
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
            RC = 1,
            length(Q1,LQ1),
            length(Q2,LQ2),
            format('   - Fail (~w/~w): ~w.~n',[LQ1,LQ2,N]),
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

diffEq((As,X1),(As,X2))
 :-
    eqX(X1,X2),
    !.


% Liberal equality test on grammar fragments

eqP(p(As,N,X1),p(As,N,X2)) :- eqX(X1,X2).

eqX(X1,X2)
 :-
    X1 =.. [F|Xs1],
    X2 =.. [F|Xs2],
    eqXs(F,Xs1,Xs2).

eqXs(true,[],[]) :- !.
eqXs(fail,[],[]) :- !.
eqXs(a,[],[]) :- !.
eqXs(t,[V],[V]) :- !.
eqXs(n,[V],[V]) :- !.
eqXs(v,[V],[V]) :- !.
eqXs(*,[X1],[X2]) :- eqX(X1,X2).
eqXs(+,[X1],[X2]) :- eqX(X1,X2).
eqXs(?,[X1],[X2]) :- eqX(X1,X2).
eqXs(s,[S,X1],[S,X2]) :- eqX(X1,X2).
eqXs(',',[Xs1],[Xs2]) :- maplist(eqX,Xs1,Xs2).
eqXs(';',[[]],[[]]).
eqXs(';',[[X1|Xs1]],[Xs2])
 :-
    append(Xs2a,[X2|Xs2b],Xs2),
    eqX(X1,X2),
    !,
    append(Xs2a,Xs2b,Xs3),
    eqXs(';',[Xs1],[Xs3]).


% Subtracting subset test on sets of expressions

subsetXs([],Xs,Xs).
subsetXs([X1|Xs1],Xs2,Xs4)
 :-
    append(Xs2a,[X2|Xs2b],Xs2),
    eqX(X1,X2),
    append(Xs2a,Xs2b,Xs3),
    !,
    subsetXs(Xs1,Xs3,Xs4).
