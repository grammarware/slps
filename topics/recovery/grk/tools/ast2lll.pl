/*

Unparse internal EBNF representation;
use the LLL format as used in GDK
(C) 2003 Ralf Laemmel, VU & CWI, Amsterdam

LLL/specification : LLL/rule</></>+;
LLL/rule          : LLL/ident <+> </> ":" LLL/disjunction ";" <->;
LLL/disjunction   : {LLL/conjunction "|"}+;
LLL/conjunction   : LLL/term+ </>;
LLL/term          : LLL/basis <> LLL/repetition?;
LLL/basis         : LLL/ident
                  | LLL/literal
		  | "%epsilon"
		  | LLL/alternation
		  | LLL/group
                  ;
LLL/repetition    : "+" | "*" | "?";
LLL/alternation   : "{" <> LLL/basis LLL/basis <> "}" <> LLL/repetition;
LLL/group         : "(" <> LLL/disjunction <> ")" ;

*/

:- prompt(_,'').
:- op(500,fx,@).


:-
   load_files(['../../lib/io.pl'
              ],
              [if(not_loaded),silent(true)]).

diagrams([]).
diagrams([Dia|Dias]) :- 
  (
    diagram(Dia)
  ;
    format(user,'Panic: LLL export failed.\n',[]),
    halt(1)
  ),
  diagrams(Dias).


diagram(dia(Cs,Exp)) :-
  n(Cs),
  space,
  @ ":",
  nl,
  space,
  space,
  space,
  exp(Exp),
  @ ";",
  nl, nl.


exp(nl) :- nl, space, space.
exp(concat(E1,E2)) :- exp(E1), space, exp(E2).
exp(t(Cs)) :- t(Cs).
exp(n(Cs)) :- n(Cs).
exp(or(epsilon,plus(E))) :- protect(E), @ "*".
exp(or(epsilon,E)) :- protect(E), @ "?".
exp(or(E1,E2)) :- @ "(", exp(E1), space, @ "|", space, orexp(E2), @ ")".
exp(plus(E)) :- protect(E), @ "+".
exp(obrace).
exp(cbrace).
exp(footnote(_)).
exp(E) :- format(user,"LLL export failed for ~w.\n",[E]), halt(1).

orexp(or(E1,E2)) :- exp(E1), space, @ "|", space, orexp(E2).
orexp(E) :- exp(E).


n(Cs0) :-
  ( append(Cs1,[0'-|Cs2],Cs0),
    \+ ( member(C,Cs2), \+ isDigit(C) )
  ;
    Cs1 = Cs0
  ),
  maplist(hyphen2underscore,Cs1,Cs3),
  format("~s",[Cs3]).

t(Cs) :- format("\"~s\"",[Cs]).
@(Cs) :- format("~s",[Cs]).


protect(E) :- protected(E), exp(E).
protect(E) :- @ "(", exp(E), @ ")".


protected(t(_)).
protected(n(_)).
protected(plus(_)).
protected(or(_,_)).
protected(nonempty(_)).


space :- write(' ').


:- 
   unix(argv(Argv)),
   append(_,[--,InFile,OutFile],Argv),
   see(InFile),
   read((_,Asts)),
   seen,
   format(user,'LLL export: ~a -> ~a ...\n',[InFile,OutFile]),
   tell(OutFile),
   diagrams(Asts),
   told,
   halt.
