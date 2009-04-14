/*

Turn AST of EBNF into DCG (Prolog)
(C) 2003 Ralf Laemmel, VU & CWI, Amsterdam

*/

:- prompt(_,'').
:- op(500,fx,@).

% Generate DCG program
diagrams(_,[]).
diagrams(Asts,[D1|Ds]) :- 
  diagram(Asts,D1),
  diagrams(Asts,Ds).

% Generate DCG rule from AST
diagram(Asts,dia(Cs,Exp)) :-
  n(Cs),
  space,
  @ "-->",
  nl,
  space,
  space,
  exp(Asts,Exp),
  nl,
  space,
  space,
  @ ".",
  nl, nl,
  !.


/*
   Generate output for EBNF expressions
*/

exp(_,true) :- 
  @ "true".

exp(Asts,concat(nl,E)) :-
  nl, space, space,
  exp(Asts,E).

exp(Asts,concat(E1,E2)) :-
  epsilon(E1),
  exp(Asts,E2).

exp(Asts,concat(E1,E2)) :-
  \+ epsilon(E1),
  \+ E1 == nl,
  epsilon(E2),
  exp(Asts,E1).

exp(Asts,concat(E1,E2)) :-
  \+ epsilon(E1),
  \+ E1 == nl,
  exp(Asts,E1),
  @ ",",
  space,
  exp(Asts,E2).

exp(_,t(Cs)) :-
  t(Cs).

exp(_,n(Cs)) :-
  n(Cs).

exp(_,cut) :-
  @ "!".

exp(Asts,test(E)) :-
  protect(Asts,E),
  @ "#".

exp(Asts,or(E1,plus(E2))) :-
  epsilon(E1),
  protect(Asts,E2),
  @ "*".

/*

More "Meta" than necessary.

exp(Asts,or(E1,E2)) :-
  epsilon(E1),
  protect(Asts,E2),
  @ "?".

*/

exp(Asts,or(E1,E2)) :-
  epsilon(E1),
  exp(Asts,or(E2,true)).

exp(Asts,or(E1,E2)) :-
  @ "(",
  exp(Asts,E1), space, @ ";", space, orexp(Asts,E2),
  @ ")".

exp(Asts,plus(E)) :-
  protect(Asts,E), @ "+".


orexp(Asts,or(E1,E2)) :-
  exp(Asts,E1),
  space,
  @ ";",
  space,
  orexp(Asts,E2).

orexp(Asts,E) :-
  \+ E = or(_,_),
  exp(Asts,E).

% Test for elements being equivalent to EPSILON
epsilon(epsilon).
epsilon(obrace).
epsilon(cbrace).
epsilon(footnote(_)).
epsilon(concat(E1,E2)) :- epsilon(E1), epsilon(E2).


% Generate output for nonterminals
n(Cs0) :-
  ( append(Cs1,[0'-|Cs2],Cs0),
    \+ ( member(C,Cs2), \+ isDigit(C) )
  ;
    Cs1 = Cs0
  ),
  maplist(hyphen2underscore,Cs1,Cs3),
  format("~s",[Cs3]).



% Generate output for terminals
t(Cs) :-
  format("@(\"~s\")",[Cs]).


% Generate output for keywords
@(Cs) :-
  format("~s",[Cs]).


% Optionally protect elements by parentheses
protect(Asts,E) :- protected(E), exp(Asts,E).
protect(Asts,E) :- @ "(", exp(Asts,E), @ ")".


% Elements that need no extra parentheses
protected(t(_)).
protected(n(_)).
protected(or(_,_)).


% Emit a space
space :- write(' ').


% Generate a line of output for file with terminals
terminal(Cs) :-
  format("terminal(\"~s\").\n",[Cs]).


:-
   load_files(['../lib/io.pl',
               '../lib/traversal.pl'
              ],
              [silent(true)]),
   unix(argv(Argv)),
   append(_,[--,AfterFile,DcgFile],Argv),
   see(AfterFile),
   read((_,Asts)),
   seen,
   format('Dumping DCG in file ~a ...\n',[DcgFile]),
   funion(t,Asts,Set),
   tell(DcgFile),
   diagrams(Asts,Asts),
   foreach(terminal,Set),
   told,
   halt.
