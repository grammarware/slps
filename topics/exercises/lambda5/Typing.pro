% See slide 183
:- ['Syntax.pro'].

% hastype(Context,Term,Type)

% T-Variable
hastype(G,var(V),Type) :-
 member([V,Type],G).

% T-Abstraction
hastype(G,lam(X,XT,T),maps(XT,U)) :-
 hastype([[X,XT]|G],T,U).

% T-Application
hastype(G,app(T,U),Type) :-
 hastype(G,U,UT),
 hastype(G,T,maps(UT,Type)).

% See slide 184

% T-True
hastype(_,true,bool).

% T-False
hastype(_,false,bool).
