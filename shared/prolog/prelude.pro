% Run the main predicate and die modestly

run
 :-
    ( 
      main
    ; 
      current_prolog_flag(argv,Argv),
      format('Error running ~w.~n',[Argv]), 
      halt(1)
    ).


% Assertion checking

require(G,F,Args)
 :-
    ( G -> true ; ( 
      write('Assertion failed: '),
      format(F,Args),
      nl,
      halt(-1) 
    ) ).


% Right-associative list fold

foldr(_,E,[],E).

foldr(O,E,[H|T],R2)
 :-
    foldr(O,E,T,R1),
    apply(O,[H,R1,R2]),
    !.


% Count the number of occurrences of an element in a list

countocc(_,[],0).
countocc(X,[X|Xs],N1) :- !, countocc(X,Xs,N0), N1 is N0 + 1.
countocc(X,[_|Xs],N0) :- countocc(X,Xs,N0).


% Concat a list of lists

concat(Ls,L) :- foldr(append,[],Ls,L).


% Find all doubles in a list

doubles(Xs,Zs)
 :-
    doubles([],Xs,Ys),
    list_to_set(Ys,Zs),
    !.

doubles(_,[],[]).
doubles(X,[H|T1],[H|T2]) :- member(H,X), doubles(X,T1,T2).
doubles(X,[H|T1],T2) :- \+ member(H,X), doubles([H|X],T1,T2).


% Filter a list by a predicate

filter(_,[],[]).
filter(P,[H|T],L2) 
 :-
    ( apply(P,[H]) -> 
          L2 = [H|L1]
        ; L2 = L1
    ),
    filter(P,T,L1).


% Left-associative, accumulating list fold

accum(_,[],A,A).

accum(G,[H|T],A1,A3)
 :-
    apply(G,[H,A1,A2]),
    accum(G,T,A2,A3),
    !.


% Accumulation and list construction

accum(_,[],A,A,[]).

accum(G,[H1|T1],A1,A3,[H2|T2])
 :-
    apply(G,[H1,A1,A2,H2]),
    accum(G,T1,A2,A3,T2),
    !.


% Visit data by term traversal

visit(G,X)
 :-
    ( apply(G,[X]); true ),
    X =.. [_|Xs],
    maplist(visit(G),Xs),
    !.


% Collect data by term traversal

collect(G,X,L2)
 :-
    ( apply(G,[X,L1]) -> true; L1 = [] ),
    X =.. [_|Xs],
    maplist(collect(G),Xs,Ys),
    concat([L1|Ys],L2),
    !.


% Transform data by term traversal

transform(G,X,Z)
 :-
    ( apply(G,[X,Y]) -> true; Y = X ),
    Y =.. [F|Ys],
    maplist(transform(G),Ys,Zs),
    Z =.. [F|Zs],
    !.

transformExcept(G1,G2,X,Z)
 :-
    ( apply(G1,[X,Y]) -> true; Y = X ),
    ( apply(G2,[Y]) -> 
          Z = Y
        ; (
            Y =.. [F|Ys],
            maplist(transformExcept(G1),Ys,Zs),
            Z =.. [F|Zs]
          )
    ),
    !.


% Repeat until no more changes

ytransform(G,X,Z)
 :-
    transform(G,X,Y),
    ( X == Y -> Z = Y; ytransform(G,Y,Z) ),
    !.


% Zip together two lists

zip([],[],[]).
zip([H1|T1],[H2|T2],[(H1,H2)|L]) :- zip(T1,T2,L).


% Unification tests

unifiable(X,Y) :- unifiable(X,Y,_).

nonunifiable(X,Y) :- \+ unifiable(X,Y).
