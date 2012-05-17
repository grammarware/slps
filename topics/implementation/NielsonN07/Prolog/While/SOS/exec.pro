/*

(C) 2010, Ralf Laemmel
Small-step operational semantics of statements in the While language.

*/


% Transitive closure

execute( (S1,M1),
         M3)
 :-
    step((S1,M1),(S2,M2)),
    execute((S2,M2),M3).

execute( (S1,M1),
         M2)
 :-
    step((S1,M1),M2),
    \+ M2 = (_,_).
 

% Empty statement

step( (skip,M),
      M).


% Sequential composition

step( (seq(S1,S2),M1),
      (seq(S3,S2),M2))
 :-
    step((S1,M1),(S3,M2)).

step( (seq(S1,S2),M1),
      (S2,M2))
 :-
    step((S1,M1),M2),
    \+ M2 = (_,_).


% Assignment

step( (assign(X,A),M1),
      M2)
 :-
    evala(A,M1,Y),
    update(M1,X,Y,M2).


% Conditional statement

step( (ifthenelse(B,S1,_),M),
      (S1,M))
 :-
    evalb(B,M,tt).

step( (ifthenelse(B,_,S2),M),
      (S2,M))
 :-
    evalb(B,M,ff).


% Loop statement

step( (while(B,S),M),
      (ifthenelse(B,seq(S,while(B,S)),skip),M)).
