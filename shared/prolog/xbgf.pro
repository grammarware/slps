:- ensure_loaded('ll.pro').


%
% Static namespace declarations
%

:- multifile sxmlns/2.

sxmlns(xlgf,'http://planet-sl.org/xlgf').


% Define a nonterminal

defineN(Ps1,G1,G2)
 :-
    usedNs(G1,Uses),
    ps2n(Ps1,N),
    require(
      member(N,Uses),
      'Nonterminal ~q must not be fresh.',
      [N]),
    newN(Ps1,N,G1,G2),
    !.


% Introduce a nonterminal

introduceN(Ps1,G1,G2)
 :-
    usedNs(G1,Uses),
    ps2n(Ps1,N),
    require(
      ( \+ member(N,Uses) ),
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
      '~q does not represent a single definition.',
      [Ps1]),
    !.

newN(Ps1,N,G1,G2)
 :-
    definedNs(G1,Defs),
    require(
      ( \+ member(N,Defs) ),
      'Definition for ~q clashes with existing definition.',
      [N]),
    G1 = g(Rs,Ps2),
    append(Ps2,Ps1,Ps3),
    G2 = g(Rs,Ps3),
    !.


% Label a production

labelP(P0,g(Rs,Ps1),g(Rs,Ps2))
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
    append(Ps1a,[p(As3,N,X)|Ps1b],Ps2),
    !.


% Massage a grammar

massageP(P0,G1,G2)
 :-
    G1 = g(Rs,Ps1),
    normalizeG_algebraically(P0,P1),
    P1 = p(As1,N1,X1),
    findP(Ps1,As1,N1,P2,Ps2a,Ps2b),
    P2 = p(As1,N1,X2),
    require(
      massageX(N1,X1,X2),
      'Halting problem remains unsovled.',
      []),
    append(Ps2a,[P1|Ps2b],Ps2),
    G2 = g(Rs,Ps2),
    require(
      ( \+ G1 == G2 ),
      'Vacous transformation detected.',
      []), 
    !.


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
massageX_selectors(s(_,X1),X2) :- massageX_selectors(X1,X2).
massageX_selectors(X1,s(_,X2)) :- massageX_selectors(X1,X2).
massageX_selectors(','(X1s),','(X2s)) :- maplist(massageX_selectors,X1s,X2s).
massageX_selectors(';'(X1s),';'(X2s)) :- maplist(massageX_selectors,X1s,X2s).
massageX_selectors('*'(X1),'*'(X2)) :- massageX_selectors(X1,X2).
massageX_selectors('+'(X1),'+'(X2)) :- massageX_selectors(X1,X2).
massageX_selectors('?'(X1),'?'(X2)) :- massageX_selectors(X1,X2).


% Inline nonterminal

inline(N,G1,G3)
 :- 
    G1 = g(Rs,Ps1),
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
    transform(inline_rule(N,X),Ps3,Ps4),
    G2 = g(Rs,Ps4),
    normalizeG(G2,G3),
    !.
    
inline_rule(N,X,n(N),X).


% Strip labels

stripLs(G1,G3)
 :-
    !,
    G1 = g(Rs,Ps1),
    maplist(stripL_rule,Ps1,Ps2),
    G2 = g(Rs,Ps2),
    require(
      ( \+ G1 == G2 ),
      'Vacous transformation detected.',
      []), 
    normalizeG(G2,G3),    
    !.

stripL(L,G1,G3)
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
    maplist(stripL_rule(L),Ps1,Ps2),
    G2 = g(Rs,Ps2),
    normalizeG(G2,G3),
    !.

stripL_rule(p(_,N,X),p([],N,X)).

stripL_rule(L,p([l(L)],N,X),p([],N,X))
 :-
    !.

stripL_rule(_,P,P).


% Strip selectors

stripSs(G1,G3)
 :-
    !,
    G1 = g(Rs,Ps1),
    transform(stripS_rule,Ps1,Ps2),
    G2 = g(Rs,Ps2),
    require(
      ( \+ G1 == G2 ),
      'Vacous transformation detected.',
      []), 
    normalizeG(G2,G3),    
    !.

stripS(S,G1,G3)
 :-
    allSs(G1,Ss),
    require(
      member(S,Ss),
      'Selector ~q must be in use.',
      [S]),
    G1 = g(Rs,Ps1),
    transform(stripS_rule(S),Ps1,Ps2),
    G2 = g(Rs,Ps2),
    normalizeG(G2,G3),
    !.

stripS_rule(s(_,X),X).

stripS_rule(S,s(S,X),X).


% Strip terminals

stripTs(G1,G3)
 :-
    transform(stripT_rule,G1,G2),
    normalizeG(G2,G3),
    !.

stripT(T,G1,G3) 
 :-
    allTs(G1,Ts),
    require(
      member(T,Ts),
      'The terminal ~q must occur.',
      [T]),
    transform(stripT_rule(T),G1,G2),
    require(
      ( \+ G1 == G2 ),
      'Vacous transformation detected.',
      []), 
    normalizeG(G2,G3),
    !.

stripT_rule(t(_),true).

stripT_rule(T,t(T),true).


% Extract a phrase

extract(P0,G1,G3)
 :-
    G1 = g(Rs,Ps1),
    normalizeG_algebraically(P0,P1),
    P1 = p(As1,N1,X1),
    findP(Ps1,As1,N1,P2,Ps3,Ps4),
    P2 = p(As1,N1,X2),
    require(
      ( \+ X1 == X2),
      'Phrases ~q and ~q must differ.',
      [X1,X2]),
    require(
      foldXs(X1,X2,N2,X3),
      'Phrases ~q and ~q do not match.',
      [X1,X2]),
    allNs(Ps1,Ns),
    require(
      ( \+ member(N2,Ns) ),
      'Nonterminal ~q must be fresh.',
      [N2]),
    append(Ps3,[p(As1,N1,X1),p([],N2,X3)|Ps4],Ps5),
    G2 = g(Rs,Ps5),
    normalizeG(G2,G3),
    !.


% Fold a phrase

fold(P0,G1,G3)
 :-
    fu(X1,X2,foldXs(X1,X2),P0,G1,G3).


% Commmon core of fold and unfold

fu(X1,X2,G,P0,G1,G3)
 :-
    G1 = g(Rs,Ps1),
    normalizeG_algebraically(P0,P1),
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
    normalizeG_algebraically(X3,X4),
    require(
      splitN(Ps1,N2,[p([],N2,X4)],_,_),
      'Nonterminal ~q must be defined as ~q.',
      [N2,X4]),
    append(Ps3,[p(As1,N1,X1)|Ps4],Ps5),
    G2 = g(Rs,Ps5),
    normalizeG(G2,G3),
    !.


% Extraction at the level of RHSs

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


% Permute RHS of production

permuteP(P0,G1,G2)
 :- 
    G1 = g(Rs,Ps1),
    normalizeG_algebraically(P0,P1),
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
      permuteXs(Xs1,Xs2),
      'Phrases ~q and ~q must be permutations of each other.',
      [X1,X2]),
    append(Ps3,[P1|Ps4],Ps5),
    G2 = g(Rs,Ps5),
    !.

permuteXs([],[]).
permuteXs([X|Xs1],Xs2)
 :-
    append(Xs2a,[X|Xs2b],Xs2),
    !,
    append(Xs2a,Xs2b,Xs3),
    permuteXs(Xs1,Xs3),
    !.


% Prune nonterminals
% (Assume epsilon as missing definition)

prune(N,G1,G3)
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
    transform(prune_rule(N),G1,G2),
    require(
      ( \+ G1 == G2 ),
      'Vacous transformation detected.',
      []), 
    normalizeG(G2,G3),
    !.
    
prune_rule(N,n(N),true).


% Remove a production

removeP(P0,G1,G3)
 :- 
    G1 = g(Rs,Ps1),
    normalizeG_algebraically(P0,P1),
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
      [N]),
    G2 = g(Rs,Ps2),
    normalizeG(G2,G3),
    !.   


% Rename a nonterminal

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
    transform(renameN_rules(N1,N2),G1,G2),
    !.

renameN_rules(N1,N2,g(Rs1,Ps),g(Rs2,Ps))
 :-
    append(Rs1a,[N1|Rs1b],Rs1),
    append(Rs1a,[N2|Rs1b],Rs2).
renameN_rules(N1,N2,n(N1),n(N2)).
renameN_rules(N1,N2,p(As,N1,X),p(As,N2,X)).


% Rename a label

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
    transform(renameL_rule(L1,L2),G1,G2),
    !.

renameL_rule(L1,L2,p(As1,N,X),p(As2,N,X))
 :-
    append(As1a,[l(L1)|As1b],As1),
    append(As1a,[l(L2)|As1b],As2).





% Rename a selector

renameS((S1,S2),G1,G2)
 :-
    renameS(S1,S2,G1,G2).

renameS(S1,S2,G1,G2)
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
    transform(renameS_rule(S1,S2),G1,G2),
    !.

renameS(L,S1,S2,G1,G2)
 :-
    G1 = g(Rs,Ps1),
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
    transform(renameS_rule(S1,S2),P1,P2),
    append(Ps2a,[P2|Ps2b],Ps3),
    G2 = g(Rs,Ps3),
    !.

renameS_rule(S1,S2,s(S1,X),s(S2,X)).


% Restrict the grammar by phrase replacement

restrictP(P0,G1,G3)
 :-
    G1 = g(Rs,Ps1),
    normalizeG_algebraically(P0,P1),
    P1 = p(As1,N1,X1),
    findP(Ps1,As1,N1,P2,Ps2a,Ps2b),
    P2 = p(As1,N1,X2),
    require(
      ( \+ X1 == X2 ),
      'The phrases ~q and ~q must be different.',
      [X1,X2]),
    require(
      restrictX(X1,X2),
      'The phrase ~q must be (algorithmically) restricted by ~q.',
      [X1,X2]),
    append(Ps2a,[P1|Ps2b],Ps3),
    G2 = g(Rs,Ps3),
    normalizeG(G2,G3),
    !.


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

restrictX(','(Xs1),','(Xs2)) :- maplist(restrictX,Xs1,Xs2).


% Unchain a nonterminal -- a restricted unfold

unchainN(N1,G1,G2)
 :-
    G1 = g(Rs,Ps1),
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
      [N1]),
    !.


% Undefine a nonterminal, i.e., remove all productions

undefine(N,G1,G3)
 :-
    definedNs(G1,Ns),
    require(
       member(N,Ns),
       'Nonterminal ~q not defined.',
       [N]),
    G1 = g(Rs1,Ps1),
    filter(nonunifiable(N),Rs1,Rs2),
    filter(nonunifiable(p(_,N,_)),Ps1,Ps2),
    G2 = g(Rs2,Ps2),
    normalizeG(G2,G3),
    !.


% Unfold a nonterminal

unfold(P0,G1,G3)
 :-
    fu(X2,X1,foldXs(X1,X2),P0,G1,G3).


% Confusing renaming, also called "unification"

uniteN(N1,N2,G1,G3)
 :-
    allNs(G1,Ns),
    require(
       ( member(N1,Ns), member(N2,Ns) ),
       'Both ~q and ~q must not be fresh.',
       [N1,N2]),
    transform(renameN_rules(N1,N2),G1,G2),
    normalizeG(G2,G3),
    !.


% Turn a choice-based definition into one based on multiple productions.

verticalN(N,G1,G3)
 :-
    G1 = g(Rs,Ps1),
    splitN(Ps1,N,Ps2,Ps2a,Ps2b),
    require(
       Ps2 = [p(As,N,';'(Xs1))],
       'Nonterminal ~q must be defined by a choice.',
       [N]),
    allLs(G1,Ls),
    maplist(vertical_rule(Ls,As,N),Xs1,Ps3),
    concat([Ps2a,Ps3,Ps2b],Ps4),
    G2 = g(Rs,Ps4),
    normalizeG(G2,G3),
    !.

verticalL(L,G1,G3)
 :-
    G1 = g(Rs,Ps1), 
    require(
      (
        append(Ps1a,[P|Ps1b],Ps1),
        P = p(As,N,X),
        member(l(L),As)
      ),
      'Label ~q not found.',
      [L]),
    allLs(G1,Ls),
    require(
      countocc(L,Ls,1),
      'Label ~q must be used exactly once.',
      [L]),
    require(
       X = ';'(Xs),
       'Production ~q must be defined by a choice.',
       [P]),
    allLs(G1,Ls),
    maplist(vertical_rule(Ls,[],N),Xs,Ps2),
    concat([Ps1a,Ps2,Ps1b],Ps3),
    G2 = g(Rs,Ps3),
    normalizeG(G2,G3),
    !.


vertical_rule(Ls,As,N,s(L,X),p([l(L)],N,X))
 :-
    \+ member(L,Ls),
    \+ member(l(_),As),
    !.

vertical_rule(As,_,N,X,p(As,N,X)).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


xlgf(T,G1,G2)
 :-
    self(name(xlgf:define),T),
    !,
    children(name(lgf:production),T,Ps1),
    maplist(xmlToP,Ps1,Ps2),
    format(' * define ~q~n',[Ps2]),
    defineN(Ps2,G1,G2),
    !.

xlgf(T,G1,G4)
 :-
    self(name(xlgf:downcase),T),
    !,

    % Downcase nonterminals

    allNs(G1,Ns1),
    filter(downcasy,Ns1,Ns2),
    format(' * downcase nonterminals ~q~n',[Ns2]),
    maplist(downcase_atom,Ns2,Ns3),
    zip(Ns2,Ns3,Ns4),
    accum(renameN,Ns4,G1,G2),

    % Downcase labels

    allLs(G2,Ls1),
    filter(downcasy,Ls1,Ls2),
    format(' * downcase labels ~q~n',[Ls2]),
    maplist(downcase_atom,Ls2,Ls3),
    zip(Ls2,Ls3,Ls4),
    accum(renameL,Ls4,G2,G3),

    % Downcase selectors

    allSs(G2,Ss1),
    filter(downcasy,Ss1,Ss2),
    format(' * downcase selectors ~q~n',[Ss2]),
    maplist(downcase_atom,Ss2,Ss3),
    zip(Ss2,Ss3,Ss4),
    accum(renameS,Ss4,G3,G4),

    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:extract),T),
    !,
    child(name(lgf:production),T,P1),
    xmlToP(P1,P2),
    format(' * extract ~q~n',[P2]),
    extract(P2,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:fold),T),
    !,
    child(name(lgf:production),T,P1),
    xmlToP(P1,P2),
    format(' * fold ~q~n',[P2]),
    fold(P2,G1,G2),
    !.

xlgf(T,G,G)
 :-
    self(name(xlgf:id),T),
    !,
    format(' * id~n',[]),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:inline),T),
    !,
    content(T,N),
    format(' * inline nonterminal ~q~n',[N]),
    inline(N,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:introduce),T),
    !,
    children(name(lgf:production),T,Ps1),
    maplist(xmlToP,Ps1,Ps2),
    format(' * introduce ~q~n',[Ps2]),
    introduceN(Ps2,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:label),T),
    !,
    child(name(lgf:production),T,P1),
    xmlToP(P1,P2),
    format(' * label ~q~n',[P2]),
    labelP(P2,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:massage),T),
    !,
    child(name(lgf:production),T,P1),
    xmlToP(P1,P2),
    format(' * massage ~q~n',[P2]),
    massageP(P2,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:permute),T),
    !,
    child(name(lgf:production),T,P1),
    xmlToP(P1,P2),
    format(' * permute ~q~n',[P2]),
    permuteP(P2,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:prune),T),
    !,
    content(T,N),
    format(' * prune nonterminal ~q~n',[N]),
    prune(N,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:remove),T),
    !,
    child(name(lgf:production),T,P1),
    xmlToP(P1,P2),
    format(' * remove ~q~n',[P2]),
    removeP(P2,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:rename),T),
    !,
    (
      child(name(label),T,X),
      F = renameL,
      C = label
    ;
      child(name(nonterminal),T,X),
      F = renameN,
      C = nonterminal
    ;
      child(name(selector),T,X),
      (
        child(name(in),X,In) ->
            ( 
              content(In,Z0),
              F = renameS(Z0)
            )
          ;
            F = renameS
      ),
      C = selector
    ),
    child(name(from),X,From),
    child(name(to),X,To),
    content(From,Z1),
    content(To,Z2),
    format(' * rename ~q ~q -> ~q~n',[C,Z1,Z2]),
    F =.. F1,
    append(F1,[Z1,Z2,G1,G2],F2),
    F3 =.. F2,
    F3,
    !.

xlgf(T,G,g(Rs2,Ps))
 :-
    self(name(xlgf:reroot),T),
    !,
    G = g(_,Ps),
    children(name(root),T,Rs1),
    maplist(content,Rs1,Rs2),
    format(' * reroot to ~q~n',[Rs2]),
    definedNs(G,Ns1),
    subtract(Rs2,Ns1,Ns2),
    require(
       subset(Rs2,Ns1),
       'Nonterminal(s) ~q not found.', 
       [Ns2]),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:restrict),T),
    !,
    child(name(lgf:production),T,P1),
    xmlToP(P1,P2),
    format(' * restrict ~q~n',[P2]),
    restrictP(P2,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:sequence),T),
    !,
    children(element,T,Ts),
    accum(xlgf,Ts,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:strip),T),
    child(name(label),T,T1),
    !,
    content(T1,L),
    format(' * strip label ~q~n',[L]),
    stripL(L,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:strip),T),
    child(name(allLabels),T,_),
    !,
    format(' * strip all labels~n',[]),
    stripLs(G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:strip),T),
    child(name(selector),T,T1),
    !,
    content(T1,S),
    format(' * strip selector ~q~n',[S]),
    stripS(S,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:strip),T),
    child(name(allSelectors),T,_),
    !,
    format(' * strip all selectors~n',[]),
    stripSs(G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:strip),T),
    child(name(terminal),T,T1),
    !,
    content(T1,T2),
    format(' * strip terminal ~q~n',[T2]),
    stripT(T,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:strip),T),
    child(name(allTerminals),T,_),
    !,
    format(' * strip all terminals~n',[]),
    stripTs(G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:unchain),T),
    !,
    content(T,N),
    format(' * unchain ~q~n',[N]),
    unchainN(N,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:undefine),T),
    !,
    content(T,N),
    format(' * undefine ~q~n',[N]),
    undefine(N,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:unfold),T),
    !,
    child(name(lgf:production),T,P1),
    xmlToP(P1,P2),
    format(' * unfold ~q~n',[P2]),
    unfold(P2,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:unite),T),
    !,
    child(name(add),T,Add),
    child(name(to),T,To),
    content(Add,N1),
    content(To,N2),
    format(' * unite ~q -> ~q~n',[N1,N2]),
    uniteN(N1,N2,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:vertical),T),
    child(name(label),T,T1),
    !,
    content(T1,L),
    format(' * vertical expansion of label ~q~n',[L]),
    verticalL(L,G1,G2),
    !.

xlgf(T,G1,G2)
 :-
    self(name(xlgf:vertical),T),
    child(name(nonterminal),T,T1),
    !,
    content(T1,N),
    format(' * vertical expansion of nonterminal ~q~n',[N]),
    verticalN(N,G1,G2),
    !.


main 
 :- 
    current_prolog_flag(argv,Argv),
    append(_,['--',LgfIn,XlgfIn,LgfOut],Argv),
    ( exists_file(LgfOut) -> delete_file(LgfOut); true ),
    load_structure(LgfIn, [G1], [dialect(xmlns)]),
    xmlToG(G1,G2),
    format(' * normalize~n',[LgfIn]),
    normalizeG(G2,G3),
    load_structure(XlgfIn, [T], [dialect(xmlns)]),
    xlgf(T,G3,G4),
    gToXml(G4,G5),
    open(LgfOut, write, OStream),
    xml_write(OStream,G5,[]),
    close(OStream),
    halt.

:- run.
