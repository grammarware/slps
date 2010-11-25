welltyped(T,A) :- welltyped([],T,A).

% We had to rewrite the typing rules for NB to incorporate the typing context.

welltyped(_,true,bool).
welltyped(_,false,bool).
welltyped(_,zero,nat).
welltyped(G,succ(T),nat) :- welltyped(G,T,nat).
welltyped(G,pred(T),nat) :- welltyped(G,T,nat).
welltyped(G,iszero(T),bool) :- welltyped(G,T,nat).
welltyped(G,if(T1,T2,T3),A) :-
 welltyped(G,T1,bool),
 welltyped(G,T2,A),
 welltyped(G,T3,A).


:- ensure_loaded('../../shared/map.pro').


% Typing rules of the lambda calculus

welltyped(G,var(X),A)
 :-
    member((X,A),G).

welltyped(G,app(T1,T2),B)
 :-
    welltyped(G,T1,fun(A,B)),
    welltyped(G,T2,A).

welltyped(G1,lam(X,A,T),fun(A,B))
 :-
    update(G1,X,A,G2),
    welltyped(G2,T,B).


% We add the fix construct.

welltyped(G,fix(T),A)
 :-
    welltyped(G,T,fun(A,A)).

