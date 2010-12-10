:- multifile erase/2.

/*

The predicate elaborate/2 basically applies erase/2 repeatedly in
bottom-up manner to a given term and all its subterms until the term
does no longer change. (We use the univ operator to take deconstruct
and reconstruct terms. We use maplist to apply elaborate/2 to a list
of terms.)

*/

elaborate(T1,T3)
 :-
    T1 =.. [F|Xs],
    maplist(elaborate,Xs,Ys),
    T12 =.. [F|Ys],
    ( erase(T12,T2) -> true; T2 = T12 ),
    !,
    ( T1 == T2 -> T3 = T2; elaborate(T2,T3) ),
    !.

