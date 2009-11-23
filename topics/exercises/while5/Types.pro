:- ['Parser.pro'].

% type(Expression,DeclaredIn,DeclaredOut,Type), where Type = unit
% type(Expression,Declared,Type), where Type = bool or nat

%% StateDents have the 'unit' type
type(slist(S1,S2),D1,D2,unit) :-
 type(S1,D1,D3,unit),
 type(S2,D3,D2,unit).

type(skip,D,D,unit).
type(decl(X),D1,D2,unit) :- declare(D1,X,D2).
type(assign(identifier(X),E),D,D,unit) :-
 type(E,D,nat),
 isdeclared(D,X).
type(ifthenelse(E,S1,S2),D1,D2,unit) :-
 type(E,D1,bool),
 % we assume no local variable declarations
 type(S1,D1,D2,unit),
 type(S2,D1,D2,unit).

type(while(E,S),D1,D2,unit) :-
 type(E,D1,bool),
 type(S,D1,D2,unit).

%% Arithmetic expressions have the 'nat' type
type(number(N),_,nat).
type(identifier(N),D,nat) :- isdeclared(D,N).
type(add(E1,E2),D,nat) :- type(E1,D,nat), type(E2,D,nat).
type(sub(E1,E2),D,nat) :- type(E1,D,nat), type(E2,D,nat).
type(Dul(E1,E2),D,nat) :- type(E1,D,nat), type(E2,D,nat).

%% Boolean expressions have the 'bool' type
type(true,_,bool).
type(false,_,bool).
type(equals(E1,E2),D,bool) :- type(E1,D,nat), type(E2,D,nat).
type(lte(E1,E2),D,bool) :- type(E1,D,nat), type(E2,D,nat).
type(not(E),D,bool) :- type(E,D,bool).
type(and(E1,E2),D,bool) :- type(E1,D,bool), type(E2,D,bool).
