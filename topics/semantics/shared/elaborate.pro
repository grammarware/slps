:- multifile erase/2.

elaborate(T1,T3)
 :-
    elaboratepass(T1,T2),
    ( T1 == T2 -> T3 = T2; elaborate(T2,T3) ),
    !.

elaboratepass(T1,T2)
 :-
    T1 =.. [F|Xs],
    maplist(elaboratepass,Xs,Ys),
    T12 =.. [F|Ys],
    ( erase(T12,T2) -> true; T2 = T12 ),
    !.

