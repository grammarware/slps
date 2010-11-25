:- ensure_loaded('freevars.pro').

substitute(N,X,var(X),N).

substitute(_,X,var(Y),var(Y))
 :-
    \+ X == Y.

substitute(N,X,app(M1,M2),app(M3,M4))
 :-
    substitute(N,X,M1,M3),
    substitute(N,X,M2,M4).

substitute(_,X,lam(X,M),lam(X,M)).

substitute(N,X,lam(Y,M1),lam(Y,M2))
 :-
    \+ X == Y,
    freevars(N,Xs),
    ( member(Y,Xs) ->
         (
           freshvar(Xs,Z),
           substitute(var(Z),Y,M1,M12),
           substitute(N,X,M12,M2)
         )
       ; substitute(N,X,M1,M2)
    ).


%
% freshvar(Xs,X): X is a variable not in Xs.
% We use numbers as generated variables.
%

freshvar(Xs,X)
 :-
    freshvar(Xs,X,0).

freshvar(Xs,N,N)
 :- 
    \+ member(N,Xs).

freshvar(Xs,X,N1)
 :- 
    member(N1,Xs),
    N2 is N1 + 1,
    freshvar(Xs,X,N2).

