%% Type system for lambda calculus, see slide 183
:- ['Syntax.pro'].

% hastype(Context,Term,Type)

% T-Variable
hastype(G,var(V),Type) :-
 member([V,Type],G).

% T-Abstraction
hastype(G,lam(X,Type,T),maps(Type,U)) :-
 hastype([[X,Type]|G],T,U).

% T-Application
hastype(G,app(T,U),Type) :-
 hastype(G,U,UT),
 hastype(G,T,maps(UT,Type)).

%% Booleans, see slide 184
% T-True
hastype(_,true,bool).

% T-False
hastype(_,false,bool).

% Naturals
hastype(_,X,nat) :- number(X).
hastype(G,succ(T),nat) :- hastype(G,T,nat).
hastype(G,pred(T),nat) :- hastype(G,T,nat).
hastype(G,iszero(T),bool) :- hastype(G,T,nat).
hastype(G,if(T1,T2,T3),Type) :-
  hastype(G,T1,bool),
  hastype(G,T2,Type),
  hastype(G,T3,Type).

% Fixed point combinator, see slide 189
% Typing rule
hastype(G,fix(T),Type) :- hastype(G,T,maps(Type,Type)).
