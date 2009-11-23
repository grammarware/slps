%% Small-step transition relation for expressions
/*
% Closure for arithmetic expressions
evala(A,M,V) :-
 evalastep(A,M,V),
 number(V).

evala(A1,M,V) :-
 evalastep(A1,M,A2),
 evala(A2,M,V).

% Closure for boolean expressions
evalb(B,M,V) :-
 evalbstep(B,M,V),
 isbool(V).

evalb(B1,M,V) :-
 evalbstep(B1,M,B2),
 evalb(B2,M,V).
*/

%% Evaluate arithmetic expressions
% Number is evaluated to its value
evalastep(number(V),_,V).

% Variable reference is evaluated to its current value
evalastep(identifier(X),M,number(Y)) :- lookup(M,X,Y).

%% Adddition
% Case 1: add number to number
evalastep(add(number(V1),number(V2)),_,number(V)) :- V is V1 + V2.
% Case 2: add number to expression
evalastep(add(number(V1),A2),M,add(number(V1),A3)) :- evalastep(A2,M,A3).
% Case 3: add expression to something
evalastep(add(A1,A2),M,add(A3,A2)) :- evalastep(A1,M,A3).

%% Subtraction
% Case 1: subtract number from number
evalastep(sub(number(V1),number(V2)),_,number(V)) :- V is V1 - V2.
% Case 2: subtract number from expression
evalastep(sub(number(V1),A2),M,sub(number(V1),A3)) :- evalastep(A2,M,A3).
% Case 3: subtract expression from something
evalastep(sub(A1,A2),M,sub(A3,A2)) :- evalastep(A1,M,A3).

%% Multiplication
% Case 1: number times number
evalastep(mul(number(V1),number(V2)),_,number(V)) :- V is V1 * V2.
% Case 2: number times expression
evalastep(mul(number(V1),A2),M,sub(number(V1),A3)) :- evalastep(A2,M,A3).
% Case 3: expression times something
evalastep(mul(A1,A2),M,sub(A3,A2)) :- evalastep(A1,M,A3).

%% Evaluate Boolean expressions
% True is tt
evalbstep(true,_,tt).

% False is ff
evalbstep(false,_,ff).

%% Negation
% One small step is enough
evalbstep(not(B),M,V) :-
 evalbstep(B,M,V1),
 isbool(V1),
 not(V1,V).
% One small step is not enough
evalbstep(not(B1),M,not(B2)) :-
 evalbstep(B1,M,B2).

%% Conjunction
% Case 1: atomic boolean and atomic boolean
evalbstep(and(B1,B2),_,V) :-
 isbool(B1),
 isbool(B2),
 and(B1,B2,V).
% Case 2: atomic boolean and non-atomic boolean
evalbstep(and(B1,B2),M,and(B1,B3)) :-
 isbool(B1),
 evalbstep(B2,M,B3).
% Case 3: non-atomic boolean and something
evalbstep(and(B1,B2),M,and(B3,B2)) :-
 evalbstep(B1,M,B3).

%% Test for equality
% Case 1: compare number with number
evalbstep(equals(V1,V2),_,V) :-
 number(V1),
 number(V2),
 equals(V1,V2,V).
% Case 2: compare number with expression
evalbstep(equals(V1,A2),M,equals(V1,A3)) :- 
 number(V1),
 evalastep(A2,M,A3).
% Case 3: compare expression with something
evalbstep(equals(A1,A2),M,equals(A3,A2)) :- evalastep(A1,M,A3).

% Test for being less than or equal
% Case 1: compare number with number
evalbstep(lte(V1,V2),_,V) :-
 number(V1),
 number(V2),
 lte(V1,V2,V).
% Case 2: compare number with expression
evalbstep(lte(V1,A2),M,lte(V1,A3)) :- 
 number(V1),
 evalastep(A2,M,A3).
% Case 3: compare expression with something
evalbstep(lte(A1,A2),M,lte(A3,A2)) :- evalastep(A1,M,A3).

%% Basic operations
equals(V1,V2,tt) :- V1 == V2.
equals(V1,V2,ff) :- \+ V1 == V2.
lte(V1,V2,tt) :- V1 =< V2.
lte(V1,V2,ff) :- \+ V1 =< V2.
not(tt,ff).
not(ff,tt).
and(tt,tt,tt).
and(ff,_,ff).
and(_,ff,ff).
isbool(tt).
isbool(ff).
