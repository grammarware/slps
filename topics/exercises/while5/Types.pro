:- ['Parser.pro'].

% type(Expression,DeclaredIn,DeclaredOut,Type), where Type = unit
% type(Expression,Declared,Type), where Type = bool or nat

%% StateDents have the 'unit' type, see slide 191
type(slist(S1,S2),D1,D2,U) :-
 wtype(S1,D1,D3,unit),
 wtype(S2,D3,D2,U).

type(skip,D,D,unit).
type(decl(X),D1,D2,unit) :- declare(D1,X,D2).
type(assign(identifier(X),E),D,D,unit) :-
 wtype(E,D,nat),
 isdeclared(D,X).
type(ifthenelse(E,S1,S2),D1,D2,U) :-
 wtype(E,D1,bool),
 % we assume no local variable declarations
 wtype(S1,D1,D2,U),
 wtype(S2,D1,D2,U).

type(while(E,S),D1,D2,unit) :-
 wtype(E,D1,bool),
 wtype(S,D1,D2,unit).

%% Arithmetic expressions have the 'nat' type
type(number(_),_,nat).
type(identifier(N),D,nat) :- isdeclared(D,N).
type(add(E1,E2),D,nat) :- wtype(E1,D,nat), wtype(E2,D,nat).
type(sub(E1,E2),D,nat) :- wtype(E1,D,nat), wtype(E2,D,nat).
type(mul(E1,E2),D,nat) :- wtype(E1,D,nat), wtype(E2,D,nat).

%% Boolean expressions have the 'bool' type
type(true,_,bool).
type(false,_,bool).
type(equals(E1,E2),D,bool) :- wtype(E1,D,nat), wtype(E2,D,nat).
type(lte(E1,E2),D,bool) :- wtype(E1,D,nat), wtype(E2,D,nat).
type(not(E),D,bool) :- wtype(E,D,bool).
type(and(E1,E2),D,bool) :- wtype(E1,D,bool), wtype(E2,D,bool).

wtype(E,D1,D2,T) :-
 type(E,D1,D2,T),
 write(E),
 write(' is of type '),
 write(T), nl.

wtype(E,D,T) :-
 type(E,D,T),
 write(E),
 write(' is of type '),
 write(T), nl.