replaceEverywhere(_,_,_,[],[],[]).
replaceEverywhere(Com,E1,E2,[Ast1|Asts1],[Ast2|Asts2],Mods1) :-
  Ast1 = dia(Cs,E3),
  replace(E1,E2,E3,E4),
  ( E3 == E4, !, E3 = E5, Mods1 = Mods
  ; normalise(E4,E5), Mods1 = [replace(Com,Cs)|Mods]
  ),
  Ast2 = dia(Cs,E5),
  replaceEverywhere(Com,E1,E2,Asts1,Asts2,Mods).


replace(Cs,E1,E2,Asts1,Asts2) :-
  append(Asts3,[dia(Cs,E3)|Asts4],Asts1),
  append(Asts3,[dia(Cs,E5)|Asts4],Asts2),
  replace(E1,E2,E3,E4),
  \+ E3 == E4,
  normalise(E4,E5).

% Full match
replace(E1,E2,E3,E4) :-
  sfreplace(E1,E2,E3,E4),
  !.

% Head of input matches; restart replacement for tail
replace(E1,E2,concat(E3a,E3b),concat(E4a,E4b)) :-
  sfreplace(E1,E2,E3a,E4a),
  replace(E1,E2,E3b,E4b),
  !.

% Handle associativity of disjunction
replace(or(E1a,E1b),E2,or(E1a,or(E1b,E3)),or(E2,E3)).

% Try list-pattern matching
replace(E1,E2,E3,E4) :-
  replacestar(E1,E1,E2,E3,E4),
  !.

% Descend into subterms
replace(E1,E2,E3,E4) :-
  E3 =.. [F3|Ps3],
  maplist(replace(E1,E2),Ps3,Ps4),
  E4 =.. [F3|Ps4].


% Full match
replacestar(_,E1,E2,E3,E4) :-
  spreplace(E1,E2,E3,E4),
  !.
 
% Head of input matches; restart replacement for tail
replacestar(E0,E1,E2,concat(E3a,E3b),concat(E4a,E4b)) :-
  spreplace(E1,E2,E3a,E4a),
  replace(E0,E2,E3b,E4b),
  !.

% Heads of E1 and E3 are the same; continue with tails
replacestar(E0,E1,E2,E3,E4) :-
  E1 = concat(P,E1b),
  E3 = concat(P,E3b),
  replacestar(E0,E1b,E2,E3b,E4),
  !.

% nl skipping
replacestar(E0,E1,E2,E3,concat(nl,E4)) :-
  \+ E1 = concat(nl,_),
  E3 = concat(nl,E3b),
  replacestar(E0,E1,E2,E3b,E4).

% nl skipping
replacestar(E0,E1,E2,E3,E4) :-
  E1 = concat(nl,E1b),
  \+ E3 = concat(nl,_),
  replacestar(E0,E1b,E2,E3,E4).



% Full equality modulo ...
sfreplace(E1,E2,E1,E2).

sfreplace(E1,E2,concat(nl,E3),concat(nl,E4)) :-
 sfreplace(E1,E2,E3,E4).

sfreplace(concat(nl,E1),E2,E3,E4) :-
 sfreplace(E1,E2,E3,E4).

sfreplace(n(Cs1),E2,n(Cs3),E2) :-
  \+ E2 = n(_),
  ( append(_,[0'-,D1],Cs1), isDigit(D1), !, fail
  ; append(Cs1,[0'-,D3],Cs3), isDigit(D3)
  ),
  !.

sfreplace(n(Cs1),n(Cs2),n(Cs3),n(Cs4)) :-
  \+ Cs1 == Cs3,
  ( append(_,[0'-,D1],Cs1), isDigit(D1), !, fail
  ; append(_,[0'-,D2],Cs2), isDigit(D2), !, fail
  ; append(Cs1,[0'-,D3],Cs3), isDigit(D3), append(Cs2,[0'-,D3],Cs4)
  ),
  !.

sfreplace(E1,E2,E3,E2) :-
  E1 =.. [F|E1s],
  E3 =.. [F|E3s],
  member(F,[concat,or,plus]),
  sfreplacelist(E1s,E3s),
  !.

sfreplacelist([],[]).
sfreplacelist([E1|E1s],[E3|E3s]) :-
  sfreplace(E1,epsilon,E3,_),
  sfreplacelist(E1s,E3s).



/*

%
% This variant leads to:
%  Arguments are not sufficiently instantiated
% In SWI-Prolog 5.1.1, 5.0.10, 5.1.12
% Cannot be traced, i.e., program works fine in traced mode. 
%

sfreplace(n(Cs1),n(Cs2),n(Cs3),n(Cs4)) :-
  \+ Cs1 == Cs3,
  \+ (append(_,[0'-,D1],Cs1), isDigit(D1)),
  \+ (append(_,[0'-,D2],Cs2), isDigit(D2)),
  append(Cs1,[0'-,D3],Cs3), isDigit(D3),
  append(Cs2,[0'-,D3],Cs4),
  !.

*/



% Prefix equality modulo ...
spreplace(E1,E2,E1,E2).

spreplace(E1,E2,concat(nl,E3),E4) :-
 spreplace(E1,E2,E3,E4).

spreplace(concat(nl,E1),E2,E3,E4) :-
 spreplace(E1,E2,E3,E4).

spreplace(n(Cs1),E2,n(Cs3),E2) :-
  \+ E2 = n(_),
  ( append(_,[0'-,D1],Cs1), isDigit(D1), !, fail
  ; append(Cs1,[0'-,D3],Cs3), isDigit(D3)
  ),
  !.

spreplace(n(Cs1),n(Cs2),n(Cs3),n(Cs4)) :-
  \+ Cs1 == Cs3,
  ( append(_,[0'-,D1],Cs1), isDigit(D1), !, fail
  ; append(_,[0'-,D2],Cs2), isDigit(D2), !, fail
  ; append(Cs1,[0'-,D3],Cs3), isDigit(D3), Cs2 = Cs4 % This is the diff. between sf and sp.
  ),
  !.

spreplace(E1,E2,E3,E2) :-
  E1 =.. [F|E1s],
  E3 =.. [F|E3s],
  member(F,[concat,or,plus]),
  spreplacelist(E1s,E3s),
  !.

spreplacelist([],[]).
spreplacelist([E1|E1s],[E3|E3s]) :-
  spreplace(E1,epsilon,E3,_),
  spreplacelist(E1s,E3s).



/*
   Removal of the empty string, e.g.:

   1. x? -> x
   2. x? y? -> x | y | x a

   This is an enabler
   for a simple approach to removing permutation phrases
   as provided by tools/nop.pl.

*/

nonempty(or(epsilon,X),X).

nonempty(concat(nl,X0),X1) :-
  nonempty(X0,X1).

nonempty(concat(X0,Y0),or(X1,or(Y1,concat(X1,Y1)))) :-
  nonempty(X0,X1),
  nonempty(Y0,Y1).

