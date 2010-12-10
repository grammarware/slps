:- multifile welltyped/3.
:- ['../../lambda/typed/welltyped.pro'].

/*
Just like with typed lambdas, we assume that the type A of the
variable X in a recursive let has to be explicitly declared in
letrec(X,A,T1,T2). We add the pair (X,A) to the typing context, and
use the updated context for type-checking both the the definition of
the recursive binding T1 and the expression T2 in which the binding is
effective.

*/

welltyped(G1,letrec(X,A,T1,T2),B)
 :-
    update(G1,X,A,G2),
    welltyped(G2,T1,A),
    welltyped(G2,T2,B).
