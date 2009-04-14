/*

Traversal functionality for GRK
(C) 2003 Ralf Laemmel, VU & CWI, Amsterdam

*/

% Append a list of lists
concat([],[]).
concat([H|T],L2) :- append(H,L1,L2),concat(T,L1).



% Full top-down traversal with recovery from failure

fulltd(G,X,Z) :-
  ( apply(G,[X,Y])
  ; X = Y
  ),
  Y =.. [F|Ps1],
  maplist(fulltd(G),Ps1,Ps2),
  Z =.. [F|Ps2],
  !.



% Full bottom-up traversal with recovery from failure

fullbu(G,X,Z) :-
  X =.. [F|Ps1],
  maplist(fulltd(G),Ps1,Ps2),
  Y =.. [F|Ps2],
  ( apply(G,[Y,Z])
  ; Y = Z
  ),
  !.



% Top-down traversal stopped by success

stoptd(G,X,Y) :-
  (
    apply(G,[X,Y])
  ;
    X =.. [F|Ps1],
    maplist(stoptd(G),Ps1,Ps2),
    Y =.. [F|Ps2]
  ),
  !.



% Bottom-up traversal with at least one hit

somebu(G,X,Y) :-
  ( some(somebu(G),X,Y)
  ; apply(G,[X,Y])
  ).



% Innermost normalisation

innermost(G,X,Z) :- somebu(G,X,Y), !, innermost(G,Y,Z).
innermost(_,X,X).



% Succeed for some immediate subterms

some(G,X,Y) :- 
  X =.. [F|Ps1],
  somelist(G,Ps1,Ps2),
  Y =.. [F|Ps2].

somelist(G,[X|Xs],[Y|Ys]) :- apply(G,[X,Y]), !, anylist(G,Xs,Ys).
somelist(G,[X|Xs],[X|Ys]) :- somelist(G,Xs,Ys).

anylist(_,[],[]).
anylist(G,[X|Xs],[Y|Ys]) :- apply(G,[X,Y]), !, anylist(G,Xs,Ys).
anylist(G,[X|Xs],[X|Ys]) :- anylist(G,Xs,Ys).



% Deep union
dunion(G,X,Ys) :- apply(G,[X,Ys]).
dunion(G,X,Ys) :- X =.. [_|Xs], unionlist(dunion(G),Xs,Ys).



% Collect all terms with a certain outermost functor
funion(F,X,[T]) :-
  X =.. [F,T].

funion(F,X,Ys) :-
  \+ X =.. [F,_],
  X =.. [_|Xs],
  unionlist(funion(F),Xs,Ys).



% Distribute a goal over a list; take union of intermediate results
unionlist(_,[],[]).
unionlist(G,[X|Xs],Ys0) :-
  apply(G,[X,Ys1]),
  unionlist(G,Xs,Ys2),
  union(Ys1,Ys2,Ys0).



% Iterate a unary goal over a list
foreach(_,[]).
foreach(G,[X|Xs]) :- apply(G,[X]), foreach(G,Xs).



% Repeat a certain goal a number of times
times(0,_).
times(N0,G) :- N1 is N0 - 1, G, times(N1,G).
