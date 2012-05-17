%% Small-step transition relation for statement execution
execute(S,M1,M2) :-
 executionstep(S,M1,M2).

execute(S1,M1,M3) :-
 executionstep(S1,M1,S2,M2),
 execute(S2,M2,M3).

% Skip statement
executionstep(skip,M,M).

%% Sequential composition
% Only a small step for S1
executionstep(slist(S1,S2),M1,slist(S1p,S2),M2) :-
 executionstep(S1,M1,S1p,M2).
% Done with S1, only S2 is left now
executionstep(slist(S1,S2),M1,S2,M2) :-
 executionstep(S1,M1,M2).
 
%% Assignment
% Right hand side is a number
executionstep(assign(identifier(X),N),M1,M2) :-
 number(N),
 update(M1,X,N,M2).
% Right hand side evaluation progresses
executionstep(assign(identifier(X),A1),M,assign(identifier(X),A2),M) :-
 evalastep(A1,M,A2).

%% Conditional statement
% True condition
executionstep(ifthenelse(tt,S1,_),M,S1,M).
% False condition
executionstep(ifthenelse(ff,_,S2),M,S2,M).
% Non-atomic condition
executionstep(ifthenelse(B1,S1,S2),M,ifthenelse(B2,S1,S2),M) :-
 evalbstep(B1,M,B2).

% Loop statement
executionstep(while(B,S),M,ifthenelse(B,slist(S,while(B,S)),skip),M).
