% Strategies

innermost(S,X,Y) :- repeat(oncebu(S),X,Y).
repeat(S,X,Z) :- apply(S,[X,Y]), repeat(S,Y,Z).
repeat(_,X,X).
oncebu(S,X,Y) :- one(oncebu(S),X,Y).
oncebu(S,X,Y) :- apply(S,[X,Y]).
one(S,X,Y)
 :-
    X =.. [F|Xs1],
    append(A,[X1|B],Xs1),
    apply(S,[X1,X2]),
    append(A,[X2|B],Xs2),
    Y =.. [F|Xs2].


% EBNF

many1(P,X,Z) :- apply(P,[X,Y]), many(P,Y,Z).
many(P,X,Y) :- many1(P,X,Y).
many(_,X,X).

many1(P,[H|T],X,Z) :- apply(P,[H,X,Y]), many(P,T,Y,Z).
many(P,L,X,Y) :- many1(P,L,X,Y).
many(_,[],X,X).

option(_,P,R,X,Y) :- apply(P,[R,X,Y]).
option(D,_,D,X,X).
