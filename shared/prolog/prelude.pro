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

soft(G,F,Args)
 :-
    ( G -> true ; ( 
      write('Assertion failed: '),
      format(F,Args),
      nl,
      write('Program continues.'),
      nl,
      !,
      fail
    ) ).

cease(F,Args)
 :-
    require(fail,F,Args).

require(G,F,Args)
 :-
    ( G -> true ; ( 
      write('Assertion failed: '),
      format(F,Args),
      nl,
      halt(-1) 
    ) ).

require_ground(T)
 :-
    require(
      ground(T),
      '~q corrupted -- not ground!',
      [T]).


% Stratego-like try

try(G,X,Y) :- (apply(G,[X,Y]); Y = X), !.


% Generalized once

once(G,X,Y) :- apply(G,[X,Y]), !.


% Right-associative list fold

foldr(_,E,[],E).

foldr(O,E,[H|T],R2)
 :-
    foldr(O,E,T,R1),
    apply(O,[H,R1,R2]).


% List-pattern matching

listmatch(L1,L2,L3,L123)
 :-
    append(L1,L23,L123),
    append(L2,L3,L23).


% Count the number of occurrences of an element in a list

countocc(_,[],0).
countocc(X,[X|Xs],N1) :- countocc(X,Xs,N0), N1 is N0 + 1.
countocc(X,[Y|Xs],N0) :- \+ X = Y, countocc(X,Xs,N0).


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
    accum(G,T,A2,A3).


% Accumulation and list construction

accum(_,[],A,A,[]).

accum(G,[H1|T1],A1,A3,[H2|T2])
 :-
    apply(G,[H1,A1,A2,H2]),
    accum(G,T1,A2,A3,T2).


% Visit data by term traversal

visit(G,X)
 :-
    once((apply(G,[X]);true)),
    X =.. [_|Xs],
    maplist(visit(G),Xs).


% Test data by term traversal

rectest(G,X)
 :-
    apply(G,[X]),
    X =.. [_|Xs],
    maplist(rectest(G),Xs).


% Negation by failure

not(G,X) :- apply(G,[X]), !, fail.
not(_,_).

not(G,X,Y) :- apply(G,[X,Y]), !, fail.
not(_,_,_).


% Collect data by term traversal

collect(G,X,L2)
 :-
    ( apply(G,[X,L1]) -> true; L1 = [] ),
    X =.. [_|Xs],
    maplist(collect(G),Xs,Ys),
    concat([L1|Ys],L2).


% Transform data by term traversal

transform(G,X,Z)
 :-
    X =.. [F|Xs],
    maplist(transform(G),Xs,Ys),
    Y =.. [F|Ys],
    apply(G,[Y,Z]).

transformWhile(G1,G2,X,Z)
 :-
    apply(G1,[X,Y]),
    ( apply(G2,[Y]) -> 
          (
            Y =.. [F|Ys],
            maplist(transformWhile(G1,G2),Ys,Zs),
            Z =.. [F|Zs]
          )
        ;
          Z = Y
    ).

stoptd(G,X,Y)
 :-
    apply(G,[X,Y])
    ; 
    ( X =.. [F|Xs],
      maplist(stoptd(G),Xs,Ys),
      Y =.. [F|Ys]
    ).


% Repeat until no more changes

ytransform(G,X,Z)
 :-
    transform(try(G),X,Y),
    ( X == Y -> Z = Y; ytransform(G,Y,Z) ).


% Zip together two lists

zip([],[],[]).
zip([H1|T1],[H2|T2],[(H1,H2)|L]) :- zip(T1,T2,L).


% Unification tests

unifiable(X,Y) :- unifiable(X,Y,_).

nonunifiable(X,Y) :- \+ unifiable(X,Y).


% Diff two terms; succeed with first difference; fail otherwise

diff(T,T,_)
 :-
    !,
    fail.

diff(T1,T2,P1)
 :-
    T1 =.. [F|L1],
    T2 =.. [F|L2],
    length(L1,Len),
    length(L2,Len),
    !,
    difflist(L1,L2,(F/Len:1),P1).

diff(_,_,[]).

difflist([H1|_],[H2|_],E,[E|P1])
 :-
    diff(H1,H2,P1),
    !.

difflist([_|T1],[_|T2],E1,P1)
 :-
    E1 = (F/Len:Pos1),
    Pos2 is Pos1 + 1,
    E2 = (F/Len:Pos2),
    difflist(T1,T2,E2,P1).


% Pretty print list

ppList(I,List)
 :-
    format('[~n',[]),
    ppElements(I,List),
    format(']',[]),
    !.

ppElement(I,E,C) :- format('~w~q~w~n',[I,E,C]).

ppElements(_,[]).

ppElements(I,[E])
 :-
    ppElement(I,E,'').

ppElements(I,[E1,E2|Es])
 :-
    ppElement(I,E1,','),
    ppElements(I,[E2|Es]).
