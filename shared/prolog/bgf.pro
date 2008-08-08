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


% Split productions into those for N and their pre- and postfix

splitN(Ps1,N,Ps2,Ps3,Ps4)
 :-
    require(
      member(p(_,N,_),Ps1),
      'Nonterminal ~q must be defined.',
      [N]),
    append(Ps3,PsRest,Ps1),
    \+ member(p(_,N,_),Ps3),
    PsRest = [p(_,N,_)|_],
    !,
    filter(unifiable(p(_,N,_)),PsRest,Ps2),
    filter(nonunifiable(p(_,N,_)),PsRest,Ps4).


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


% Normalization

normalizeG(Z1,Z3)
 :-
    normalizeG_grouping(Z1,Z2),
    normalizeG_algebraically(Z2,Z3),
    !.


% All productions for a nonterminal are held contiguously.

normalizeG_grouping(g(Rs,Ps1),g(Rs,Ps2))
 :-
    !,
    normalizePs_grouping(Ps1,Ps2).

% Succeed if not applied to an entire grammar
normalizeG_grouping(Z,Z).


normalizePs_grouping([],[]).
normalizePs_grouping([P|Ps1],Ps5)
 :-
    P = p(_,N,_),
    filter(unifiable(p(_,N,_)),Ps1,Ps2),
    filter(nonunifiable(p(_,N,_)),Ps1,Ps3),
    normalizePs_grouping(Ps3,Ps4),
    append([P|Ps2],Ps4,Ps5),
    !.


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
normalize_algebraically_rules(';'(Xs1),';'(Xs2))
 :-
    append(Xs1a,[fail|Xs1b],Xs1),
    append(Xs1a,Xs1b,Xs2).


% Pretty print grammar

ppG(g(Rs,Ps))
 :-
    format('g( ~q, [~n',[Rs]),
    ppPs(Ps),
    format('])~n',[]),
    !.

ppP(P,C) :- format('  ~q~w~n',[P,C]).

ppPs([]).
ppPs([P]) :- ppP(P,'').
ppPs([P1,P2|Ps]) :- ppP(P1,','), ppPs([P2|Ps]).
