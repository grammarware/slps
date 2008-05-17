evaluate(P,E,I)
 :-
    evaluate(P,[],E,I).

evaluate(_,_,literal(I),I).

evaluate(_,M,argument(N),I)
 :-
    member((N,I),M).

evaluate(P,M,binary(O,X,Y),I)
 :-
    evaluate(P,M,X,I1),
    evaluate(P,M,Y,I2),
    ( O == equal,
      ( I1 =:= I2, I = -1
      ; I1 =\= I2, I = 0
      )
    ; O == plus, I is I1 + I2
    ; O == minus, I is I1 - I2
    ).

evaluate(P,M,ifThenElse(X,Y,Z),I)
 :-
    evaluate(P,M,X,I1),
    ( I1 =\= 0, E = Y
    ; I1 =:= 0, E = Z
    ),
    evaluate(P,M,E,I).

evaluate(P,M1,apply(N,Es),I)
 :-
    member(((N,Ns),E),P),
    maplist(bindarg(P,M1),Ns,Es,M2),
    evaluate(P,M2,E,I).

bindarg(P,M,N,E,(N,I)) 
 :-
    evaluate(P,M,E,I).
