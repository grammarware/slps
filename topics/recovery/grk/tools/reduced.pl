/*

Reducedness checker for EBNF
(C) 2003 Ralf Laemmel, VU & CWI, Amsterdam

*/

:-
   load_files(['../../lib/io.pl',
               '../../lib/traversal.pl'
              ],
              [if(not_loaded),silent(true)]).



% Messages to report on lack of reducedness
top(X)    :- format("Top nonterminal ~s.\n",[X]).
bottom(X) :- format("Bottom nonterminal ~s.\n",[X]).



% Nonterminal references
referenced(X,Ys) :- dunion(nonterminal,X,Ys).
nonterminal(n(Cs0),[Cs1]) :-
  ( append(Cs1,[0'-,D],Cs0), isDigit(D)
  ; append(Cs1,[0'-,D1,D2],Cs0), isDigit(D1), isDigit(D2)
  ; Cs1 = Cs0
  ).



% Nonterminal declarations
declared([],[]).
declared([dia(Cs,_)|Asts],[Cs|Css]) :- declared(Asts,Css).



% Nonterminal references that are reachable in one step
step([],_,[]).
step([dia(Cs,Exp)|Asts],Xs,Css0) :-
  member(Cs,Xs),
  !,
  referenced(Exp,Css1),
  step(Asts,Xs,Css2),
  union(Css1,Css2,Css0).
step([_|Asts],Xs,Css0) :-
  step(Asts,Xs,Css0).


% Transitive closure of reachable nonterminals
transitive(Asts,Roots,In,Out) :-
  step(Asts,Roots,Step),
  diff(Step,In,Tmp),
  \+ Tmp == [],
  union(In,Tmp,More),
  transitive(Asts,Tmp,More,Out).

transitive(_,_,X,X).



% Set difference; should be in stdlib
diff([],_,[]).
diff([X|Xs],Ys,Zs) :- member(X,Ys), !, diff(Xs,Ys,Zs).
diff([X|Xs],Ys,[X|Zs]) :- diff(Xs,Ys,Zs).



:- 
   unix(argv(Argv)),
   append(_,[--,CheckFile,Top0|Bottoms0],Argv),
   maplist(name,Bottoms0,Bottoms1),
   name(Top0,Top1),
   see(CheckFile),
   read((_,Asts)),
   seen,
   format('Checking ~a for reducedness ...\n',[CheckFile]),
   referenced(Asts,Refs),
   declared(Asts,Decs),
   diff(Refs,Decs,Bottoms2),
   diff(Bottoms2,Bottoms1,Bottoms3),
   !,
   foreach(bottom,Bottoms3),
   transitive(Asts,[Top1],[Top1],Reachs),
   diff(Decs,Reachs,Tops),
   foreach(top,Tops),
   (
     Bottoms3 == [],
     ( Tops == []
     ; Tops == [Top1]
     ),
     halt
   ;
     halt(1)
   ).
