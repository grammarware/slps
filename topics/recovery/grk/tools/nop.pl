/*

Elimination of permutation phrases
by liberalising them, e.g., A || B becomes ( A | B )*.

(C) 2003 Ralf Laemmel, VU & CWI, Amsterdam

*/

:-
   load_files(['../../lib/traversal.pl'
              ],
              [if(not_loaded),silent(true)]).


% Rewrite step which does the work
nop(Asts,p(E1,E2),or(epsilon,plus(or(E3,E4)))) :-
  nophelper(Asts,E1,E3),
  nophelper(Asts,E2,E4).

nophelper(Asts,p(E1,E2),or(E3,E4)) :-
  nophelper(Asts,E1,E3),
  nophelper(Asts,E2,E4).

nophelper(Asts,or(epsilon,plus(E)),E) :-
  nonempty(Asts,[],E).

nophelper(Asts,or(epsilon,E),E) :-
  \+ E = plus(_),
  nonempty(Asts,[],E).

nophelper(Asts,E,E) :-
  \+ E = p(_,_),
  nonempty(Asts,[],E).


% Test for a phrase not be empty
nonempty(_,_,t(_)).
nonempty(Asts,S,n(Cs)) :-
  append(_,[dia(Cs,E)|_],Asts),
  \+ member(Cs,S),
  nonempty(Asts,[Cs|S],E).

nonempty(Asts,_,n(Cs)) :-
  \+ append(_,[dia(Cs,_)|_],Asts).
 
nonempty(Asts,S,concat(E1,E2)) :- 
    nonempty(Asts,S,E1)
  ; nonempty(Asts,S,E2).

nonempty(Asts,S,or(E1,E2)) :- 
  nonempty(Asts,S,E1),
  nonempty(Asts,S,E2).

nonempty(Asts,S,plus(E)) :-
  nonempty(Asts,S,E).



:- 
   unix(argv(Argv)),
   append(_,[--,InFile,OutFile],Argv),
   see(InFile),
   read((Mods,Asts1)),
   seen,
   format(user,'Eliminating permutation phrases: ~a -> ~a ...\n',[InFile,OutFile]),
   (
     stoptd(nop(Asts1),Asts1,Asts2)
   ;
     format(user,'Panic: permutation phrase contains epsilon operand.',[]),
     halt(1)
   ),
   tell(OutFile),
   write((Mods,Asts2)),
   write('.'),
   nl,
   told,
   halt.
