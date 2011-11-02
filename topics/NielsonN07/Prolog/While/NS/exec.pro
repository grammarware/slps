/*

(C) 2010, Ralf Laemmel
Natural semantics of statements in the While language

*/

% Skip statement

execute(skip,M,M).


% Sequential composition

execute(seq(S1,S2),M1,M3) :-
 execute(S1,M1,M2),
 execute(S2,M2,M3).


% Assignment

execute(assign(X,A),M1,M2) :-
 evala(A,M1,Y),
 update(M1,X,Y,M2).


% Conditional statement with true condition

execute(ifthenelse(B,S1,_),M1,M2) :-
 evalb(B,M1,tt),
 execute(S1,M1,M2).


% Conditional statement with false condition

execute(ifthenelse(B,_,S2),M1,M2) :-
 evalb(B,M1,ff),
 execute(S2,M1,M2).


% Loop statement with true condition

execute(while(B,S),M1,M3) :-
 evalb(B,M1,tt),
 execute(S,M1,M2),
 execute(while(B,S),M2,M3).


% Loop statement with false condition

execute(while(B,_),M,M) :-
 evalb(B,M,ff).
