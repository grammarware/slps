:- ['Library.pro'].

optimize(E1,E2)
 :-
    innermost(step,E1,E2).

step(binary(plus,X,literal(0)),X).
step(binary(plus,literal(0),X),X).
step(binary(plus,literal(I),literal(J)),literal(R)) :- R is I + J.
step(binary(minus,X,literal(0)),X).
step(binary(minus,literal(0),X),X).
step(binary(minus,literal(I),literal(J)),literal(R)) :- R is I - J.
step(binary(equal,X,X),literal(-1)).
step(binary(equal,X,Y),literal(0)) :- \+ X = Y.
step(ifThenElse(literal(0),_,X),X).
step(ifThenElse(literal(I),X,_),X) :- \+ I = 0.
step(ifThenElse(_,X,X),X).
