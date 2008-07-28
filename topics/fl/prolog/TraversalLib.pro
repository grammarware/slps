% Traversal strategies

everywhere(S,X,Z)
 :-
    all(everywhere(S),X,Y),
    apply(S,[Y,Z]).

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

all(S,X,Y)
 :-
    X =.. [F|Xs1],
    maplist(S,Xs1,Xs2),
    Y =.. [F|Xs2].
