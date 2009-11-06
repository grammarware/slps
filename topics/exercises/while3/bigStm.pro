%% Big-step transition relation for statement execution

% Skip statement
execute(skip,M,M).

% Sequential composition
execute(slist(S1,S2),M1,M3) :-
 execute(S1,M1,M2),
 execute(S2,M2,M3).

% Assignment
execute(assign(identifier(X),A),M1,M2) :-
 evala(A,M1,Y),
 update(M1,X,Y,M2).

% Conditional statement
execute(ifthenelse(B,S1,S2),M1,M2) :-
 evalb(B,M1,X),
 (
  X == tt,
  execute(S1,M1,M2)
 ;
  X == ff,
  execute(S2,M1,M2)
 ).

% Loop statement
execute(while(B,S),M1,M3) :-
 evalb(B,M1,X),
 (
  X == tt,
  execute(S,M1,M2),
  execute(while(B,S),M2,M3)
 ;
  X == ff,
  M3 = M1
 ).

