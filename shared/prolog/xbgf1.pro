:- module(xbgf1,[transformG/3]).

%
% Convenience wrapper for transformations.
% Make sure that transformations apply to normalized grammars.
% Make sure that vacouous transformations are rejected.
%

transformG(T,G1,G3)
 :-
    T =.. [F|_],
    format('Applying ~q transformation.~n',[F]),
    normalizeG(G1,G2),
    apply(T,[G2,G3]),
    require(
      ( \+ G2 == G3 ),
      'Vacuous transformation detected.',
      []),
    !.


%
% p([l(add)], f, n(p))
%
% Add a production to an existing definition
%


%
% p([l(define)], f, +n(p))
%
% Define a nonterminal
%

define(Ps1,G1,G2)
 :-
    usedNs(G1,Uses),
    ps2n(Ps1,N),
    require(
      member(N,Uses),
      'Nonterminal ~q must not be fresh.',
      [N]),
    newN(Ps1,N,G1,G2),
    !.

ps2n(Ps1,N)
 :-
    maplist(arg(2),Ps1,Ns1),
    list_to_set(Ns1,Ns2), 
    require(
      Ns2 = [N],
      'Multiple defined nonterminals found.',
      []),
    !.


%
% p([l(downcase)], f, true)
%
% Establish lowercase for all sorts of symbols
%

downcase(G1,G4) 
 :-
    % Downcase nonterminals

    allNs(G1,Ns1),
    filter(downcasy,Ns1,Ns2),
    format(' * downcase nonterminals ~q~n',[Ns2]),
    maplist(downcase_atom,Ns2,Ns3),
    zip(Ns2,Ns3,Ns4),
    accum(xbgf1:renameN,Ns4,G1,G2),

    % Downcase labels

    allLs(G2,Ls1),
    filter(downcasy,Ls1,Ls2),
    format(' * downcase labels ~q~n',[Ls2]),
    maplist(downcase_atom,Ls2,Ls3),
    zip(Ls2,Ls3,Ls4),
    accum(xbgf1:renameL,Ls4,G2,G3),

    % Downcase selectors

    allSs(G2,Ss1),
    filter(downcasy,Ss1,Ss2),
    format(' * downcase selectors ~q~n',[Ss2]),
    maplist(downcase_atom,Ss2,Ss3),
    zip(Ss2,Ss3,Ss4),
    accum(xbgf1:renameS,Ss4,G3,G4).


%
% p([l(eliminate)], f, n(n))
%
% Eliminate a defined, otherwise unused nonterminal
%


%
% p([l(extract)], f, n(p))
%
% Extract a nonterminal definition
%

extract(P1,g(Rs,Ps1),g(Rs,Ps5))
 :-
    P1 = p(As1,N1,X1),
    findP(Ps1,As1,N1,P2,Ps3,Ps4),
    P2 = p(As1,N1,X2),
    require(
      ( \+ X1 == X2),
      'Phrases ~q and ~q must differ.',
      [X1,X2]),
    require(
      xbgf1:foldXs(X1,X2,N2,X3),
      'Phrases ~q and ~q do not match.',
      [X1,X2]),
    allNs(Ps1,Ns),
    require(
      ( \+ member(N2,Ns) ),
      'Nonterminal ~q must be fresh.',
      [N2]),
    append(Ps3,[p(As1,N1,X1),p([],N2,X3)|Ps4],Ps5).


%
% p([l(fold)], f, n(p))
%
% Fold an expression to its defining nonterminal
%

fold(P0,G1,G3)
 :-
    fu(X1,X2,foldXs(X1,X2),P0,G1,G3).


% Commmon core of fold and unfold

fu(X1,X2,G,P1,G1,G3)
 :-
    G1 = g(Rs,Ps1),
    P1 = p(As1,N1,X1),
    findP(Ps1,As1,N1,P2,Ps3,Ps4),
    P2 = p(As1,N1,X2),
    require(
      ( \+ X1 == X2),
      'Phrases ~q and ~q must differ.',
      [X1,X2]),
    require(
      apply(G,[N2,X3]),
      'Phrases ~q and ~q do not match.',
      [X1,X2]),
    require(
      splitN(Ps1,N2,[p([],N2,X3)],_,_),
      'Nonterminal ~q must be defined as ~q.',
      [N2,X3]),
    append(Ps3,[p(As1,N1,X1)|Ps4],Ps5),
    G2 = g(Rs,Ps5),
    normalizeG(G2,G3),
    !.

foldXs(X1,X2,N,X3)
 :-
    \+ X1 = ','(_),
    !,
    foldXs(','([X1]),X2,N,X3).

foldXs(X1,X2,N,X3)
 :-
    \+ X2 = ','(_), 
    !,
    foldXs(X1,','([X2]),N,X3).

foldXs(','([X|X1s]),','([X|X2s]),N,X3)
 :-
    !,
    foldXs(','(X1s),','(X2s),N,X3).

foldXs(','([n(N)|X1s]),','(X2s),N,','(X3s))
 :-
    !,
    append(X3s,X1s,X2s).

foldXs(','([X1|X1s]),','([X2|X1s]),N,X3s)
 :-
    !,
    foldX(X1,X2,N,X3s).

foldX(n(N),X,N,X)
 :-
    !.

foldX(s(S,X1),s(S,X2),N,X)
 :-
    !,
    foldX(X1,X2,N,X).

foldX('?'(X1),'?'(X2),N,X)
 :-
    !,
    foldX(X1,X2,N,X).

foldX('*'(X1),'*'(X2),N,X)
 :-
    !,
    foldX(X1,X2,N,X).

foldX('+'(X1),'+'(X2),N,X)
 :-
    !,
    foldX(X1,X2,N,X).


%
% p([l(horizontal)], f, n(n))
%
% Turn multiple productions into choice
%


%
% p([l(id)], f, true)
%
% Identity
%

id(G,G).


%
% p([l(inline)], f, n(n))
%
% Inline a nonterminal definition (and eliminate it)
%

inline(N,g(Rs,Ps1),g(Rs,Ps4))
 :- 
    require(
       (\+ member(N,Rs) ),
       'Nonterminal ~q must not be root.',
       [N]),
    usedNs(Ps1,Uses1),
    require(
      member(N,Uses1),
      'Nonterminal ~q must be used.',
      [N]),
    splitN(Ps1,N,Ps2,Ps2a,Ps2b),
    require(
       Ps2 = [p(_,_,X)],
       'Nonterminal ~q must be defined horizontally.',
       [N]),
    usedNs(Ps2,Uses2),
    require(
      ( \+ member(N,Uses2) ),
      'Nonterminal ~q must not be used in its definition.',
      [N]),
    append(Ps2a,Ps2b,Ps3),
    transform(xbgf1:inline_rule(N,X),Ps3,Ps4).
    
inline_rule(N,X,n(N),X).

%
% p([l(introduce)], f, +n(p))
%
% Add a definition for a fresh nonterminal
%

introduce(Ps1,G1,G2)
 :-
    usedNs(G1,Uses),
    ps2n(Ps1,N),
    require(
      ( \+ member(N,Uses) ),
      'Nonterminal ~q must not be fresh.',
      [N]),
    new(Ps1,N,G1,G2).

new(Ps1,N,G1,G2)
 :-
    definedNs(G1,Defs),
    require(
      ( \+ member(N,Defs) ),
      'Definition for ~q clashes with existing definition.',
      [N]),
    G1 = g(Rs,Ps2),
    append(Ps2,Ps1,Ps3),
    G2 = g(Rs,Ps3).


%
% p([l(label)], f, n(p))
%
% Label a production
%

label(P0,g(Rs,Ps1),g(Rs,Ps2))
 :-
    normalizeG_algebraically(P0,P1),
    P1 = p(As1,N,X),
    require(
       ( member(l(_),As1) ),
       'Production ~q must be labeled.',
       [P1]),
    require(
      ( P2 = p(As2,N,X),
        append(Ps1a,[P2|Ps1b],Ps1),
        \+ member(l(_),As2) 
      ),
      'Production ~q (w/o label) not found.',
      [P1]),
    append(As1,As2,As3),
    append(Ps1a,[p(As3,N,X)|Ps1b],Ps2).

%
% p([l(massage)], f, n(p))
%
% Obviously correct rewrites
%

massage(P1,g(Rs,Ps1),g(Rs,Ps2))
 :-
    P1 = p(As1,N1,X1),
    findP(Ps1,As1,N1,P2,Ps2a,Ps2b),
    P2 = p(As1,N1,X2),
    require(
      xbgf1:massageX(N1,X1,X2),
      'Halting problem remains unsovled.',
      []),
    append(Ps2a,[P1|Ps2b],Ps2).


% Special rule for binary expressions

massageX(
  N,
  ','([n(N),X,n(N)]), 
  ','([n(N),'*'(','([X,n(N)]))]))
 :- !.

% Special strategy for selector renaming/dropping/adding

massageX(_,X1,X2)
 :-
    massageX_selectors(X1,X2).

massageX_selectors(X,X).

massageX_selectors(s(_,X1),X2)
 :- 
    massageX_selectors(X1,X2).

massageX_selectors(X1,s(_,X2))
 :-
    massageX_selectors(X1,X2).

massageX_selectors(','(X1s),','(X2s))
 :- 
    maplist(xbgf1:massageX_selectors,X1s,X2s).

massageX_selectors(';'(X1s),';'(X2s))
 :-
    maplist(xbgf1:massageX_selectors,X1s,X2s).

massageX_selectors('*'(X1),'*'(X2))
 :-
    massageX_selectors(X1,X2).

massageX_selectors('+'(X1),'+'(X2))
 :-
    massageX_selectors(X1,X2).

massageX_selectors('?'(X1),'?'(X2))
 :-
    massageX_selectors(X1,X2).


%
% p([l(permute)], f, n(p))
%
% Permute the body of a production
%

permute(P1,g(Rs,Ps1),g(Rs,Ps5))
 :- 
    P1 = p(As1,N1,X1),
    require(
      X1 = ','(Xs1),
      'Permutation requires a sequence instead of ~q.',
      [X1]),
    findP(Ps1,As1,N1,P2,Ps3,Ps4),
    P2 = p(As1,N1,X2),
    require(
      X2 = ','(Xs2),
      'Permutation requires a sequence instead of ~q.',
      [X2]),
    require(
      xbgf1:permuteXs(Xs1,Xs2),
      'Phrases ~q and ~q must be permutations of each other.',
      [X1,X2]),
    append(Ps3,[P1|Ps4],Ps5).

permuteXs([],[]).
permuteXs([X|Xs1],Xs2)
 :-
    append(Xs2a,[X|Xs2b],Xs2),
    !,
    append(Xs2a,Xs2b,Xs3),
    permuteXs(Xs1,Xs3),
    !.


%
% p([l(project)], f, n(p))
%
% Apply projection to the body of a production
%

%
% p([l(prune)], f, n(n))
%
% Prune nonterminals
% (Assume epsilon as missing definition)
%

prune(N,G1,G2)
 :-
    definedNs(G1,Ds),
    require(
       ( \+ member(N,Ds) ),
       'Nonterminal ~q must not be defined.',
       [N]),
    usedNs(G1,Us),
    require(
       member(N,Us),
       'Nonterminal ~q must be in use.',
       [N]),
    transform(xbgf1:prune_rule(N),G1,G2).
    
prune_rule(N,n(N),true).


% p([l(relax)], f, n(p))

% p([l(relabel)], f, n(p))


%
% p([l(remove)], f, n(p))
%
% Remove a production
%

remove(P1,g(Rs,Ps1),g(Rs,Ps2))
 :- 
    P1 = p(_,N,_),
    require(
      append(Ps1a,[P1|Ps1b],Ps1),
      'Production ~q not found.',
      [P1]),
    append(Ps1a,Ps1b,Ps2),
    definedNs(Ps2,Ns),
    require(
      member(N,Ns),
      'Nonrterminal ~q must remain defined.',
      [N]).   


%
% p([l(renameL)], f, ','([n(l), n(l)]))
% p([l(renameN)], f, ','([n(n), n(n)]))
% p([l(renameS)], f, ','([?(n(l)), n(s), n(s)]))
% p([l(renameT)], f, ','([n(t), n(t)]))
%
% Rename labels, nonterminals, selectors, and terminals
%

renameL((L1,L2),G1,G2)
 :-
    renameL(L1,L2,G1,G2).

renameL(L1,L2,G1,G2)
 :-
    allLs(G1,Ls),
    require(
       member(L1,Ls),
       'Source name ~q for renaming must not be fresh.',
       [L1]),
    require(
      countocc(L1,Ls,1),
      'Label ~q must be unique.',
      [L1]),
    require(
       (\+ member(L2,Ls)),
       'Target name ~q for renaming must be fresh.',
       [L2]),
    transform(xbgf1:renameL_rule(L1,L2),G1,G2).

renameL_rule(L1,L2,p(As1,N,X),p(As2,N,X))
 :-
    append(As1a,[l(L1)|As1b],As1),
    append(As1a,[l(L2)|As1b],As2).

renameN((N1,N2),G1,G2)
 :-
    renameN(N1,N2,G1,G2).

renameN(N1,N2,G1,G2)
 :-
    allNs(G1,Ns),
    require(
       member(N1,Ns),
       'Source name ~q for renaming must not be fresh.',
       [N1]),
    require(
       (\+ member(N2,Ns)),
       'Target name ~q for renaming must be fresh.',
       [N2]),
    transform(xbgf1:renameN_rules(N1,N2),G1,G2).

renameN_rules(N1,N2,g(Rs1,Ps),g(Rs2,Ps))
 :-
    append(Rs1a,[N1|Rs1b],Rs1),
    append(Rs1a,[N2|Rs1b],Rs2).
renameN_rules(N1,N2,n(N1),n(N2)).
renameN_rules(N1,N2,p(As,N1,X),p(As,N2,X)).

renameS((S1,S2),G1,G2)
 :-
    renameS([],S1,S2,G1,G2).

renameS([],S1,S2,G1,G2)
 :-
    allSs(G1,Ss),
    require(
       member(S1,Ss),
       'Source name ~q for renaming must not be fresh.',
       [S1]),
    require(
       (\+ member(S2,Ss)),
       'Target name ~q for renaming must be fresh.',
       [S2]),
    transform(xbgf1:renameS_rule(S1,S2),G1,G2).

renameS([L],S1,S2,g(Rs,Ps1),g(Rs,Ps3))
 :-
    splitL(Ps1,L,P1,Ps2a,Ps2b),
    allSs(P1,Ss),
    require(
       member(S1,Ss),
       'Source name ~q for renaming must not be fresh.',
       [S1]),
    require(
       (\+ member(S2,Ss)),
       'Target name ~q for renaming must be fresh.',
       [S2]),
    transform(xbgf1:renameS_rule(S1,S2),P1,P2),
    append(Ps2a,[P2|Ps2b],Ps3).

renameS_rule(S1,S2,s(S1,X),s(S2,X)).


%
% p([l(reroot)], f, *(n(n)))
%
% Assign new roots to the grammar
%

reroot(Rs,g(_,Ps),g(Rs,Ps))
 :-
    definedNs(Ps,Ns1),
    subtract(Rs,Ns1,Ns2),
    require(
       subset(Rs,Ns1),
       'Nonterminal(s) ~q not found.', 
       [Ns2]).


%
% p([l(restrict)], f, n(p))
%
% Restrict the grammar by expression replacement
%

restrict(P1,g(Rs,Ps1),g(Rs,Ps3))
 :-
    P1 = p(As1,N1,X1),
    findP(Ps1,As1,N1,P2,Ps2a,Ps2b),
    P2 = p(As1,N1,X2),
    require(
      ( \+ X1 == X2 ),
      'The phrases ~q and ~q must be different.',
      [X1,X2]),
    require(
      xbgf1:restrictX(X1,X2),
      'The phrase ~q must be (algorithmically) restricted by ~q.',
      [X1,X2]),
    append(Ps2a,[P1|Ps2b],Ps3).


% Anything trivially restricts itself.

restrictX(X,X).    

% Anything trivially restricts "any".

restrictX(_,a).

% Epsilon trivially restricts anything.

restrictX(true,_).

% "*"

restrictX('*'(X1),'*'(X2)) :- restrictX(X1,X2).
restrictX('+'(X1),'*'(X2)) :- restrictX(X1,X2).
restrictX('?'(X1),'*'(X2)) :- restrictX(X1,X2).
restrictX(X1,'*'(X2)) :- restrictX(X1,X2).

% "+"

restrictX('+'(X1),'+'(X2)) :- restrictX(X1,X2).
restrictX(X1,'+'(X2)) :- restrictX(X1,X2).

% "?"

restrictX('?'(X1),'?'(X2)) :- restrictX(X1,X2).
restrictX(X1,'?'(X2)) :- restrictX(X1,X2).

% Restriction while using selectors to "sync"

restrictX(s(S,X1),s(S,X2)) :- restrictX(X1,X2).

% ","

restrictX(','(Xs1),','(Xs2))
 :-
    maplist(xbgf1:restrictX,Xs1,Xs2).


% p([l(sequence)], f, *(n(f)))

sequence(Ts,G1,G2)
 :-
    accum(xbgf1:transformG,Ts,G1,G2).


%
% p([l(skip)], f, n(p))
%
% Skip a production
%

skip(P1,g(Rs,Ps1),g(Rs,Ps2))
 :- 
    require(
      P1 = p(_,N,n(N)),
      'Production ~q is not skippable.',
      [P1]),
    require(
      append(Ps1a,[P1|Ps1b],Ps1),
      'Production ~q not found.',
      [P1]),
    append(Ps1a,Ps1b,Ps2).


%
% p([l(stripL)], f, n(l))
% p([l(stripLs)], f, true)
% p([l(stripS)], f, n(s))
% p([l(stripSs)], f, true)
% p([l(stripT)], f, n(t))
% p([l(stripTs)], f, true)
%
% Strip labels, selectors, and terminals
%

stripL(L,G1,g(Rs,Ps2))
 :-
    allLs(G1,Ls),
    require(
      member(L,Ls),
      'Label ~q must be in use.',
      [L]),
    require(
      countocc(L,Ls,1),
      'Label ~q must be unique.',
      [L]),
    G1 = g(Rs,Ps1),
    maplist(xbgf1:stripL_rule(L),Ps1,Ps2).

stripLs(g(Rs,Ps1),g(Rs,Ps2))
 :-
    maplist(xbgf1:stripL_rule,Ps1,Ps2).

stripL_rule(p(_,N,X),p([],N,X)).

stripL_rule(L,p([l(L)],N,X),p([],N,X))
 :-
    !.

stripL_rule(_,P,P).

stripS(S,G1,g(Rs,Ps2))
 :-
    allSs(G1,Ss),
    require(
      member(S,Ss),
      'Selector ~q must be in use.',
      [S]),
    G1 = g(Rs,Ps1),
    transform(xbgf1:stripS_rule(S),Ps1,Ps2).

stripSs(g(Rs,Ps1),g(Rs,Ps2))
 :-
    transform(xbgf1:stripS_rule,Ps1,Ps2).

stripS_rule(s(_,X),X).

stripS_rule(S,s(S,X),X).

stripTs(G1,G2)
 :-
    transform(xbgf1:stripT_rule,G1,G2).

stripT(T,G1,G2) 
 :-
    allTs(G1,Ts),
    require(
      member(T,Ts),
      'The terminal ~q must occur.',
      [T]),
    transform(xbgf1:stripT_rule(T),G1,G2).

stripT_rule(t(_),true).

stripT_rule(T,t(T),true).


%
% p([l(unchain)], f, n(n))
%
% Unchain a nonterminal -- a restricted unfold
%

unchain(N1,g(Rs,Ps1),G2)
 :-
    splitN(Ps1,N1,Ps2,Ps2a,Ps2b),
    append(Ps2a,Ps2b,Ps3),
    require(
       (\+ Ps2 == [] ),
       'Nonterminal ~q must be defined.',
       [N1]),
    require(
       (\+ member(N1,Rs) ),
       'Nonterminal ~q must not be root.',
       [N1]),
    require(
       Ps2 = [p(_,_,X)],
       'Nonterminal ~q must be defined horizontally.',
       [N1]),
    require(
       append(Ps3a,[p(As1,N2,n(N1))|Ps3b],Ps3),
       'Nonterminal ~q must be used within a chain production.',
       [N1]),
    (
      member(l(_),As1) ->
          As2 = As1
        ; As2 = [l(N1)] 
    ),
    append(Ps3a,[p(As2,N2,X)|Ps3b],Ps4),
    G2 = g(Rs,Ps4),
    allNs(G2,Ns),
    require(
      (\+ member(N1,Ns) ),
      'Nonterminal ~q must appear occur exactly once.',
      [N1]).


%
% p([l(undefine)], f, n(n))
%
% Undefine a nonterminal, i.e., remove all productions
%

undefine(N,G1,g(Rs2,Ps2))
 :-
    definedNs(G1,Ns),
    require(
       member(N,Ns),
       'Nonterminal ~q not defined.',
       [N]),
    G1 = g(Rs1,Ps1),
    filter(nonunifiable(N),Rs1,Rs2),
    filter(nonunifiable(p(_,N,_)),Ps1,Ps2).


%
% p([l(unfold)], f, n(p))
%
% Unfold a nonterminal in a production
%

unfold(P1,G1,G2)
 :-
    fu(X2,X1,foldXs(X1,X2),P1,G1,G2).


%
% p([l(unite)], f, ','([n(n), n(n)]))
%
% Confusing renaming, also called "unification"
%

unite(N1,N2,G1,G2)
 :-
    allNs(G1,Ns),
    require(
       ( member(N1,Ns), member(N2,Ns) ),
       'Both ~q and ~q must not be fresh.',
       [N1,N2]),
    transform(xbgf1:renameN_rules(N1,N2),G1,G2).


%
% p([l(verticalL)], f, n(l))
% p([l(verticalN)], f, n(n))
%
% Turn choices into definitions of multiple productions
%

verticalL(L,g(Rs,Ps1),g(Rs,Ps3))
 :-
    require(
      (
        append(Ps1a,[P|Ps1b],Ps1),
        P = p(As,N,X),
        member(l(L),As)
      ),
      'Label ~q not found.',
      [L]),
    allLs(Ps1,Ls),
    require(
      countocc(L,Ls,1),
      'Label ~q must be used exactly once.',
      [L]),
    require(
       X = ';'(Xs),
       'Production ~q must be defined by a choice.',
       [P]),
    maplist(xbgf1:vertical_rule(Ls,[],N),Xs,Ps2),
    concat([Ps1a,Ps2,Ps1b],Ps3).

verticalN(N,g(Rs,Ps1),g(Rs,Ps4))
 :-
    splitN(Ps1,N,Ps2,Ps2a,Ps2b),
    require(
       Ps2 = [p(As,N,';'(Xs1))],
       'Nonterminal ~q must be defined by a choice.',
       [N]),
    allLs(Ps1,Ls),
    maplist(xbgf1:vertical_rule(Ls,As,N),Xs1,Ps3),
    concat([Ps2a,Ps3,Ps2b],Ps4).

vertical_rule(Ls,As,N,s(L,X),p([l(L)],N,X))
 :-
    \+ member(L,Ls),
    \+ member(l(_),As),
    !.

vertical_rule(As,_,N,X,p(As,N,X)).
