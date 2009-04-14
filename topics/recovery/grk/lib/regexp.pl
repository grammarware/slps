/*

Regular expressions for scanning and parsing in GRK
(C) 2003 Ralf Laemmel, VU & CWI, Amsterdam

*/

:- op(400,xf,(?)).
:- op(400,xf,(*)).
:- op(400,xf,(+)).
:- op(400,xf,(#)).
:- op(400,fx,(@)).
:- op(600,xfx,(..)).
:- op(900,fx,(~)).

:-
   load_files(['traversal.pl'
              ],
              [silent(true)]).



/*
   DCG-like interpretation of regular-expression operators
*/

true(Cs,Cs).

'!'(Cs,Cs). % No way to deal with nested cuts.

','(G1,G2,Cs1,Cs3) :- apply(G1,[Cs1,Cs2]), apply(G2,[Cs2,Cs3]).

';'(G,_,Cs1,Cs2) :- apply(G,[Cs1,Cs2]).
';'(_,G,Cs1,Cs2) :- apply(G,[Cs1,Cs2]).

?(G,Cs1,Cs2) :- ';'(G,true,Cs1,Cs2).

*(G,Cs1,Cs2) :- ?(G+,Cs1,Cs2).

+(G,Cs1,Cs2) :- ','(G,G*,Cs1,Cs2).

#(G,Cs1,Cs1) :- apply(G,[Cs1,_]).


/*
   Parse a regular expression
   by consuming a prefix of the input string
   with general backtracking enabled. 
*/

btlex(true,[],Cs,Cs).

btlex(space,[32],[32|Cs],Cs).

btlex(eof,[-1],[],[]).

btlex(nl,[10],[nl(Current)|Cs],Cs) :-
  flag(line,OldMax,0),
  ( Current >= OldMax, 
    flag(line,_,Current)
  ;
    Current <  OldMax,
    flag(line,_,OldMax)
  ),
  % write(Current), nl,
  !.

btlex(C,[C],[C|Cs],Cs).

btlex(Cs0,Cs0,Cs1,Cs2) :-
  append(Cs0,Cs2,Cs1).

btlex(C1..C2,[C],[C|Cs],Cs) :-
  \+ C = nl(_),
  C >= C1, C =< C2.

btlex(~G,[C],[C|Cs1],Cs1) :-
  \+ btlex(G,_,[C|Cs1],_).

btlex((G1,G2),Z,Cs1,Cs3) :-
  btlex(G1,X,Cs1,Cs2),
  btlex(G2,Y,Cs2,Cs3),
  append(X,Y,Z).

btlex((G;_),X,Cs1,Cs2) :-
  btlex(G,X,Cs1,Cs2).

btlex((_;G),X,Cs1,Cs2) :-
  btlex(G,X,Cs1,Cs2).

btlex(G?,X,Cs1,Cs2) :-
  btlex((G;true),X,Cs1,Cs2).

btlex(G*,X,Cs1,Cs2) :-
  btlex((G+)?,X,Cs1,Cs2).

btlex(G+,X,Cs1,Cs2) :-
  btlex((G,G*),X,Cs1,Cs2).

btlex(G#,[],Cs1,Cs1) :-
  btlex(G,_,Cs1,_).


/*

  By the virtue of a cut we 
  enforce a maximum consuption discipline
  without allowing backtracking to dive into prefixes.

*/

maxlex(G,Cs0,Cs1,Cs2) :-
  btlex(G,Cs0,Cs1,Cs2), !.
