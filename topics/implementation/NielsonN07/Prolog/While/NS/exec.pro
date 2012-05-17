/*

(C) 2010, Ralf Laemmel
Natural semantics of statements in the While language

*/

% Skip statement

exec(skip,M,M).


% Sequential composition

exec(seq(S1,S2),M1,M3) :-
 exec(S1,M1,M2),
 exec(S2,M2,M3).


% Assignment

exec(assign(X,A),M1,M2) :-
 evala(A,M1,Y),
 update(M1,X,Y,M2).


% Conditional statement with true condition

exec(ifthenelse(B,S1,_),M1,M2) :-
 evalb(B,M1,tt),
 exec(S1,M1,M2).


% Conditional statement with false condition

exec(ifthenelse(B,_,S2),M1,M2) :-
 evalb(B,M1,ff),
 exec(S2,M1,M2).


% Loop statement with true condition

exec(while(B,S),M1,M3) :-
 evalb(B,M1,tt),
 exec(S,M1,M2),
 exec(while(B,S),M2,M3).


% Loop statement with false condition

exec(while(B,_),M,M) :-
 evalb(B,M,ff).
