:- multifile welltyped/3.
:- ['../../lambda/typed/welltyped.pro'].

welltyped(G1,letrec(X,A,T1,T2),B)
 :-
    update(G1,X,A,G2),
    welltyped(G2,T1,A),
    welltyped(G2,T2,B).

