% Sequential combinator

t(X:E,E,X,_).


% Summation combinator

t(E1+_,E2,X,S) :- t(E1,E2,X,S).
t(_+E1,E2,X,S) :- t(E1,E2,X,S).


% Composition

t(E1|E2,E3|E2,X,S) :- t(E1,E3,X,S).

t(E1|E2,E1|E3,X,S) :- t(E2,E3,X,S).

t(E1|E2,E3|E4,tau,S)
 :-
    t(E1,E3,X1,S),
    t(E2,E4,X2,S),
    samename(X1,X2).


% Agent variables

t(var(A),E2,X,S)
 :-
    member((A,E1),S),
    t(E1,E2,X,S).


% Restriction

t(restrict(E1,L),restrict(E2,L),X,S) 
 :- 
    t(E1,E2,X,S),
    oklabel(X,L).


% Relate corresponding names

samename(name(N),coname(N)).
samename(coname(N),name(N)).


% Check label to be not restricted

oklabel(tau,_).
oklabel(X,L) :- basename(X,N), \+ member(N,L).


% Retrieve basename of name and co-name

basename(name(N),N).
basename(coname(N),N).

