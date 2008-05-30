:- ['TraversalLib.pro'].

optimize(E1,E2)
 :-
    innermost(step,E1,E2).

step(binary(plus,X,literal(0)),X).
step(binary(plus,literal(0),X),X).
step(binary(plus,literal(I),literal(J)),literal(R)) :- R is I + J.
step(binary(minus,literal(0),X),X).
step(binary(minus,literal(I),literal(J)),literal(R)) :- R is I - J.
step(binary(equal,X,X),literal(-1)).
step(binary(equal,X,Y),literal(0)) :- \+ X = Y.
step(ifThenElse(literal(0),_,X),X).
step(ifThenElse(literal(I),X,_),X) :- \+ I = 0.
step(ifThenElse(_,X,X),X).
step(ifThenElse(binary(equal,X,literal(0)),Y,Z),ifThenElse(X,Z,Y)).
step(ifThenElse(binary(equal,literal(0),X),Y,Z),ifThenElse(X,Z,Y)).


% 
% Innermost normalization makes sure that the rewrite rules for the
% optimization have been "exhausted". Depending on the rewrite rules
% and the sample term, a bottom-up traversal (cf. everywhere) may 
% be sufficient.
%

optimize_variation(E1,E2)
 :-
    everywhere(step_or_succeed,E1,E2).

% Argument strategy of everywhere better succeeds all the time.

step_or_succeed(X,Y) :- step(X,Y).
step_or_succeed(X,X).
