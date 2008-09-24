:- module(xbgf2,[transformT/3]).
:- use_module('xbgf1.pro').

transformT(sequence(Xs),T1,T2)
 :-
     !,
     accum(xbgf2:transformT,Xs,T1,T2),
     !.

transformT(Xbgf,r(G1,T1),T4)
 :-
    xbgf1:transformG(Xbgf,G1,G2),
    !,
    apply(Xbgf,[(G1,T1),(G2,T2)]),
    !,
    ytransform(xbgf2:normalizeT_rules,T2,T3),
    T4 = r(G2,T3),
    !,
    checkbtf(T4),
    !.

normalizeT_rules(','([]),true).
normalizeT_rules(','([Z]),Z).
normalizeT_rules('+'(true),true).
normalizeT_rules('+'([true]),true).
normalizeT_rules('*'(true),true).
normalizeT_rules('*'([true]),true).
normalizeT_rules('?'(true),true).
normalizeT_rules('?'([true]),true).
normalizeT_rules(','(Ts1),','(Ts5))
 :-
    append(Ts2,[','(Ts3)|Ts4],Ts1),
    concat([Ts2,Ts3,Ts4],Ts5).
normalizeT_rules(','(Ts1),','(Ts2))
 :-
    append(Ts1a,[true|Ts1b],Ts1),
    append(Ts1a,Ts1b,Ts2).


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

define(Ps,(_,T1),(_,T2))
 :-
    accum(xbgf2:define_strategy,Ps,T1,T2).

define_strategy(P,T1,T2)
 :-
    transform(xbgf2:define_rules(P),T1,T2).

define_rules(P1,n(P2,T1),n(P1,T2))
 :-
    P1 = p(_,N,X1),
    P2 = p(_,N,X2),
    !,
    (
      X1 == X2, !,
      T2 = T1
    ;
      X1 == v(string), X2 == v(int), !,
      T1 = v(int(V1)),
      T2 = v(string(V3)),
      number_chars(V1,V2),
      atom_chars(V3,V2)
    ;
      X2 == v(string), X1 == v(int), !,
      T1 = v(string(V1)),
      T2 = v(int(V3)),
      atom_chars(V1,V2), 
      number_chars(V3,V2)
    ).

define_rules(_,X,X).


%
% p([l(designate)], f, n(p))
%
% Label a production
%

designate(P1,(_,T1),(_,T2))
 :-
    transform(try(xbgf2:designate_rule(P1)),T1,T2).

designate_rule(P1,n(P2,T),n(P1,T))
 :-
    P1 = p([l(_)],N,X),
    P2 = p([],N,X).


%
% p([l(distributeL)], f, n(l))
% p([l(distributeN)], f, n(n))
%
% Distribute sequential composition over choices
%

distributeL(L,(_,T1),(_,T2))
 :-
    transform(try(xbgf2:distributeL_rule(L)),T1,T2).

distributeL_rule(
  L,
  n(p([l(L)],N,X),T1),
  n(p([l(L)],N,';'(Xs)),T2)
)
 :-
    distribute(X,Xs,T1,T2).

distributeN(N,(_,T1),(_,T2))
 :-
    transform(try(xbgf2:distributeN_rule(N)),T1,T2).

distributeN_rule(
  N,
  n(p(As,N,X),T1),
  n(p(As,N,';'(Xs)),T2)
)
 :-
    distribute(X,Xs,T1,T2).
  
distribute(X1,Xs,T1,';'(X2,T2))
 :-
    require(
      xbgf1:distribute_x(X1,Xs),
      'Grammar-level distribution failed ~q.',
      [X1]),
    require(
      ( 
        member(X2,Xs),
        xbgf2:distribute_t(X2,T1,T2)
      ),
      'Tree-level distribution failed ~q/~q.',
      [X2,T1]).

distribute_t(X,T,T)
 :-
    X =.. [F|_],
    T =.. [F|_],
    member(F,[true,s,t,n,a,v,?,+,*]).

distribute_t(X,';'(_,T1),T2)
 :-
    distribute_t(X,T1,T2).

distribute_t(','(Xs),','(Ts1),','(Ts2))
 :-
    maplist(xbgf2:distribute_t,Xs,Ts1,Ts2).


%
% p([l(eliminate)], f, n(n))
%
% Eliminate a defined, otherwise unused nonterminal
%

eliminate(_,(_,T),(_,T)).


%
% p([l(extract)], f, n(p))
%
% Extract a nonterminal definition
%


%
% p([l(fold)], f, n(p))
%
% Fold an expression to its defining nonterminal
%


%
% p([l(horizontal)], f, n(n))
%
% Turn multiple productions into choice
%


%
% p([l(inline)], f, n(n))
%
% Inline a nonterminal definition (and eliminate it)
%

inline(N,(G,T1),(_,T2)) 
 :-
    splitN1(G,N,p(_,_,X),_,_),
    transform(try(xbgf2:inline_rules(N,X)),T1,T2).

inline_rules(N,X,n(N),X).
inline_rules(N,_,n(p(_,N,_),T),T).


%
% p([l(introduce)], f, +n(p))
%
% Add a definition for a fresh nonterminal
%

introduce(_,(_,T1),(_,T1)).


%
% p([l(lassoc)], f, n(p))
%
% Interpret separator list left-associatively
%

lassoc(P1,(_,T1),(_,T2))
 :-
    transform(try(xbgf2:lassoc_rule(P1)),T1,T2).

lassoc_rule(P1,n(P2,','([T1,'*'(Ts)])),T2)
 :-
    P1 = p(As,N,X1),
    P2 = p(As,N,X2),
    xbgf1:assoc_rule(N,X1,X2),
    lassoc_strategy(P2,Ts,T1,T2).

lassoc_strategy(_,[],T,T).
lassoc_strategy(P,[','([Ta,Tb])|Ts],T1,T2)
 :-
    lassoc_strategy(P,Ts,n(P,','([T1,Ta,Tb])),T2).


%
% p([l(massage)], f, (n(x),n(x)))
% p([l(massageL)], f, (n(x),n(x),n(l)))
% p([l(massageN)], f, (n(x),n(x),n(n)))
%
% Semantics-preserving expression replacement
%

massage(X1,X2,(_,T1),(_,T2)) 
 :-
    btf2x(T1,X3),
    RX = xbgf2:grammar_replace(X1,X2),
    RT = xbgf2:tree_massage(X1,X2),
    tree_strategy((RX,RT),[],[],(X3,T1),(_,T2)).

massageL(X1,X2,L,(_,T1),(_,T2)) 
 :-
    btf2x(T1,X3),
    RX = xbgf2:grammar_replaceL(X1,X2,L),
    RT = xbgf2:tree_massageL(X1,X2,L),
    tree_strategy((RX,RT),[],[],(X3,T1),(_,T2)).

massageN(X1,X2,N,(_,T1),(_,T2)) 
 :-
    btf2x(T1,X3),
    RX = xbgf2:grammar_replaceN(X1,X2,N),
    RT = xbgf2:tree_massageN(X1,X2,N),
    tree_strategy((RX,RT),[],[],(X3,T1),(_,T2)).


% ------------------------------------------------------------

% Massage-based replace relations on derivation trees

tree_massage(X1,X2,_,_,(X1,T1),(X2,T2))
 :- 
    massage_bothways((X1,T1),(X2,T2)).

tree_massageN(X1,X2,N,[N],_,(X1,T1),(X2,T2))
 :-
    massage_bothways((X1,T1),(X2,T2)).

tree_massageL(X1,X2,L,_,[l(L)],(X1,T1),(X2,T2))
 :- 
    massage_bothways((X1,T1),(X2,T2)).


% ------------------------------------------------------------

massage_bothways((X1,T1),(X2,T2)) :- massage_rules((X1,T1),(X2,T2)), !.
massage_bothways((X1,T1),(X2,T2)) :- massage_rules((X2,T2),(X1,T1)), !.

massage_rules((X1,T1),(X2,T2))
 :-
    massage_s_rule(X1,X2,T1,T2).

massage_rules((X1,T1),(s(S,X2),s(S,T2)))
 :-
    member(F,[*,+,?]),
    X1 =.. [F,s(S,X)],
    X2 =.. [F,X],
    T1 =.. [F,Ts1],
    T2 =.. [F,Ts2],
    maplist(xbgf2:massage_s_rule(s(S,X),X),Ts1,Ts2).

massage_rules((?(X),?(Ts)),(';'(L),';'(Y,T)))
 :- 
    length(L,2),
    member(X,L),
    member(true,L),
    ( Ts = [], Y = true, T = true
    ; Ts = [T], Y = X
    ).

massage_rules((*(X),*(Ts)),(';'(L),';'(Y,T))) 
 :- 
    length(L,2),
    member(+(X),L),
    member(true,L),
    ( Ts = [], Y = true, T = true
    ; Ts = [_|_], Y = +(X), T = +(Ts)
    ).

massage_rules((+(X),+([T|Ts])),(','([X,*(X)]),','([T,*(Ts)]))).

massage_s_rule(s(S,X),X,s(S,T),T).


%
% p([l(permute)], f, n(p))
%
% Permute the body of a production
%

permute(p(_,N,','(Xs)),(_,T1),(_,T2))
 :-
    transform(try(xbgf2:permute_rule(N,Xs)),T1,T2).

permute_rule(N,Xs1,
  n(p(As,N,','(Xs2)),','(Ts1)),
  n(p(As,N,','(Xs1)),','(Ts2)))
 :-
    permuteXs(Xs2,Xs1,Ts1,Ts2).

permuteXs([],[],[],[]).
permuteXs(Xs1,[X|Xs2],Ts1,[T|Ts2])
 :-
    append(Xs1a,[X|Xs1b],Xs1),
    length(Xs1a,Len),
    append(Xs1a,Xs1b,Xs3),
    append(Ts1a,[T|Ts1b],Ts1),
    length(Ts1a,Len),
    append(Ts1a,Ts1b,Ts3),
    permuteXs(Xs3,Xs2,Ts3,Ts2),
    !.


%
% p([l(project)], f, n(p))
%
% Apply projection to the body of a production
%

project(p(_,N,','(Xs)),(_,T1),(_,T2))
 :-
    transform(try(xbgf2:project_rule(N,Xs)),T1,T2).

project_rule(N,Xs1,
  n(p(As,N,','(Xs2)),','(Ts1)),
  n(p(As,N,','(Xs1)),','(Ts2)))
 :-
    projectXs(Xs2,Xs1,Ts1,Ts2).

projectXs([],[],[],[])
 :-
    !.

projectXs([X|Xs1],[X|Xs2],[T|Ts1],[T|Ts2])
 :-
    !,
    projectXs(Xs1,Xs2,Ts1,Ts2).

projectXs([_|Xs1],Xs2,[_|Ts1],Ts2)
 :-
    !,
    projectXs(Xs1,Xs2,Ts1,Ts2).


%
% p([l(rassoc)], f, n(p))
%
% Interpret separator list right-associatively
%

rassoc(P1,(_,T1),(_,T2))
 :-
    transform(try(xbgf2:rassoc_rules(P1)),T1,T2).

rassoc_rules(P1,n(P2,','([T1,'*'(Ts)])),T2)
 :-
    P1 = p(As,N,X1),
    P2 = p(As,N,X2),
    xbgf1:assoc_rule1(N,X1,X2),
    rassoc_strategy1(P1,Ts,T1,T2).

rassoc_rules(P1,n(P2,+(Ts)),T1)
 :-
    P1 = p(As,N,X1),
    P2 = p(As,N,X2),
    xbgf1:assoc_rule2(N,X1,X2),
    rassoc_strategy2(P1,Ts,T1).

rassoc_strategy1(_,[],T,T).
rassoc_strategy1(P,[','([Ta,Tb])|Ts],T1,T2)
 :-
    rassoc_strategy1(P,Ts,n(P,','([T1,Ta,Tb])),T2).

rassoc_strategy2(_,[T],T).
rassoc_strategy2(P,[T1,T2|Ts],n(P,','([T1,T3])))
 :-
    rassoc_strategy2(P,[T2|Ts],T3).


%
% p([l(remove)], f, n(p))
%
% Remove a production
%


%
% p([l(renameL)], f, ','([n(l), n(l)]))
% p([l(renameN)], f, ','([n(n), n(n)]))
% p([l(renameS)], f, ','([?(n(l)), n(s), n(s)]))
% p([l(renameT)], f, ','([n(t), n(t)]))
%
% Rename labels, nonterminals, selectors, and terminals
%

renameL((L1,L2),(_,T1),(_,T2))
 :-
    renameL(L1,L2,T1,T2).

renameL(L1,L2,(_,T1),(_,T2))
 :-
    transform(try(xbgf1:renameL_rule(L1,L2)),T1,T2).

renameN((N1,N2),(_,T1),(_,T2))
 :-
    renameN(N1,N2,T1,T2).

renameN(N1,N2,(_,T1),(_,T2))
 :-
    transform(try(xbgf1:renameN_rules(N1,N2)),T1,T2).

renameS((S1,S2),(_,T1),(_,T2))
 :-
    renameS([],S1,S2,T1,T2).

renameS([],S1,S2,(_,T1),(_,T2))
 :-
    transform(try(xbgf1:renameS_rule(S1,S2)),T1,T2).

renameS([L],S1,S2,(_,T1),(_,T2))
 :-
    transform(try(xbgf2:renameS_rule(L,S1,S2)),T1,T2).

renameS_rule(L,S1,S2,n(P1,T1),n(P2,T2))
 :-
    P1 = p([l(L)],_,_),
    transform(try(xbgf1:renameS_rule(S1,S2)),P1,P2),
    transformWhile(
      try(xbgf1:renameS_rule(S1,S2)),
      xbgf2:renameS_scope,
      T1, 
      T2).

renameS_scope(T) :- \+ T = n(_,_).


%
% p([l(reroot)], f, *(n(n)))
%
% Assign new roots to the grammar
%

reroot(_,(_,T1),(_,T1)).


%
% p([l(narrow)], f, n(p))
%
% Narrow the grammar by expression replacement
%


%
% p([l(skip)], f, n(p))
%
% Skip a production
%

skip(P,(_,T1),(_,T2))
 :-
    transform(try(xbgf2:skip_rule(P)),T1,T2).

skip_rule(P,n(P,T),T).


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

stripL(L,(_,T1),(_,T2))
 :-
    transform(try(xbgf1:stripL_rules(L)),T1,T2).

stripLs((_,T1),(_,T2))
 :-
    transform(try(xbgf1:stripSs_rule),T1,T2).

stripS(S,(_,T1),(_,T2))
 :-
    transform(try(xbgf1:stripS_rule(S)),T1,T2).

stripSs((_,T1),(_,T2))
 :-
    transform(try(xbgf1:stripSs_rule),T1,T2).

stripT(T,(_,T1),(_,T2))
 :-
    transform(try(xbgf1:stripT_rule(T)),T1,T2).

stripTs((_,T1),(_,T2)) 
 :-
    transform(try(xbgf1:stripTs_rule),T1,T2).


%
% p([l(unchain)], f, n(p))
%
% Unchain a production -- a restricted unfold
%

unchain(P,(_,T1),(_,T2))
 :-
    transform(try(xbgf2:unchain_rule(P)),T1,T2).

unchain_rule(
    P1,
    n(P1,n(P2,T)),
    n(p(As2,N1,X),T))
 :-
    P1 = p(As1,N1,n(N2)),
    P2 = p(_,N2,X),
    unchain_label(As1,N2,As2).

unchain_label([],N,[l(N)]).
unchain_label([l(L)],_,[l(L)]).


%
% p([l(undefine)], f, n(n))
%
% Undefine a nonterminal, i.e., remove all productions
%

undefine(N,(_,T),(_,T))
 :-
    visit(xbgf2:undefine_rule(N),T).

undefine_rule(N1,n(P,_))
 :-
    P = p(_,N2,_),
    require(
      ( \+ N1 == N2 ),
      'Undefined nonterminal used.',
      [N1]).


%
% p([l(unfold)], f, n(p))
%
% Unfold a nonterminal in a production
%


%
% p([l(unite)], f, ','([n(n), n(n)]))
%
% Confusing renaming, also called "unification"
%


%
% p([l(verticalL)], f, n(l))
% p([l(verticalN)], f, n(n))
%
% Turn choices into definitions of multiple productions
%

verticalL(L,(_,T1),(_,T2))
 :-
    transform(try(xbgf2:verticalL_rule(L)),T1,T2).

verticalL_rule(
  L,
  n(p([l(L)],N,';'(Xs)),T1),
  n(P,T2)
)
 :-
    vertical(N,Xs,P,T1,T2).

verticalN(N,(_,T1),(_,T2))
 :-
    transform(try(xbgf2:verticalN_rule(N)),T1,T2).

verticalN_rule(
  N,
  n(p(_,N,';'(Xs)),T1),
  n(P,T2)
)
 :-
    vertical(N,Xs,P,T1,T2).
  
vertical(N,Xs,P,';'(X,T1),T2)
 :-
    member(X,Xs),
    vertical_rules(N,X,P,T1,T2).

vertical_rules(N,s(S,X),p([l(S)],N,X),s(S,T),T).
vertical_rules(N,X,p([],N,X),T,T) :- \+ X = s(_,_).


% ------------------------------------------------------------

% Top-down type propagation, bottom-up rewriting
% at the level of grammar expressions

grammar_strategy(RX,Ns,Ls,X1,X3)
 :-
    once(xbgf2:grammar_recurse(RX,Ns,Ls,X1,X2)),
    once(try(apply(RX,[Ns,Ls]),X2,X3)).

grammar_recurse(_,_,_,X,X)
 :-
    member(X,[true,a,v(_),t(_),n(_)]).

grammar_recurse(RX,Ns,Ls,s(S,X1),s(S,X2))
 :-
    grammar_strategy(RX,Ns,Ls,X1,X2).

grammar_recurse(RX,Ns,Ls,FX1,FX2)
 :-
    member(F,[*,+,?]),
    FX1 =.. [F,X1],
    FX2 =.. [F,X2],
    grammar_strategy(RX,Ns,Ls,X1,X2).

grammar_recurse(RX,Ns,Ls,','(Xs1),','(Xs2))
 :-
    maplist(xbgf2:grammar_strategy(RX,Ns,Ls),Xs1,Xs2).

grammar_recurse(RX,Ns,Ls,';'(Xs1),';'(Xs2))
 :-
    maplist(xbgf2:grammar_strategy(RX,Ns,Ls),Xs1,Xs2).

grammar_recurse(_,_,_,X1,_)
 :-
    X1 =.. [FX|_], 
    cease('Grammar rewriting failed at ~q.',[FX]).


% ------------------------------------------------------------

% Replace relations
% at the level of grammar expressions

grammar_replace(X1,X2,_,_,X1,X2).

grammar_replaceN(X1,X2,N,[N],_,X1,X2).

grammar_replaceL(X1,X2,L,_,[l(L)],X1,X2).


% ------------------------------------------------------------

% Top-down type propagation, bottom-up rewriting
% at the level of derivation trees, and hence coupled

tree_strategy((RX,RT),Ns,Ls,(X1,T1),(X3,T3))
 :-
    once(xbgf2:tree_recurse((RX,RT),Ns,Ls,(X1,T1),(X2,T2))),
    once(try(apply(RT,[Ns,Ls]),(X2,T2),(X3,T3))).

tree_recurse(_,_,_,(X,T),(X,T))
 :-
    member(X,[true,a,v(_),t(_)]).

tree_recurse(Rs,_,_,(n(N),n(P1,T1)),(n(N),n(P2,T2)))
 :-
    P1 = p(Ls,N,X1),
    P2 = p(Ls,N,X2), 
    tree_strategy(Rs,[N],Ls,(X1,T1),(X2,T2)).

tree_recurse(Rs,Ns,Ls,(s(S,X1),s(S,T1)),(s(S,X2),s(S,T2)))
 :-
    tree_strategy(Rs,Ns,Ls,(X1,T1),(X2,T2)).

tree_recurse((RX,RT),Ns,Ls,(FX1,FTs1),(FX2,FTs2))
 :-
    member(F,[*,+,?]),
    FX1 =.. [F,X1],
    FX2 =.. [F,X2],
    FTs1 =.. [F,Ts1],
    FTs2 =.. [F,Ts2],
    xbgf2:grammar_strategy(RX,Ns,Ls,X1,X2),
    length(Ts1,Len),
    length(Ts2,Len),
    repeat(Len,X1,Xs1),
    repeat(Len,X2,Xs2),
    zip(Xs1,Ts1,XTs1),
    zip(Xs2,Ts2,XTs2),
    maplist(xbgf2:tree_strategy((RX,RT),Ns,Ls),XTs1,XTs2).

tree_recurse(Rs,Ns,Ls,(','(Xs1),','(Ts1)),(','(Xs2),','(Ts2)))
 :-
    zip(Xs1,Ts1,XTs1),
    zip(Xs2,Ts2,XTs2),
    maplist(xbgf2:tree_strategy(Rs,Ns,Ls),XTs1,XTs2).

tree_recurse((RX,RT),Ns,Ls,(';'(Xs1),';'(X1,T1)),(';'(Xs2),';'(X2,T2)))
 :-
    maplist(xbgf2:grammar_strategy(RX,Ns,Ls),Xs1,Xs2),
    tree_strategy((RX,RT),Ns,Ls,(X1,T1),(X2,T2)).

tree_recurse(_,_,_,(X1,T1),_)
 :-
    X1 =.. [FX|_], 
    T1 =.. [FT|_], 
    cease('Tree rewriting failed at ~q/~q.',[FX,FT]).
