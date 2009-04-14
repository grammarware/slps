/*

Unparse internal EBNF representation
(C) 2003 Ralf Laemmel, VU & CWI, Amsterdam

*/

:- prompt(_,'').
:- op(500,fx,@).


diagrams(_,_,[]).
diagrams(Scratch,Ext,[D1|Ds]) :- 
  diagram(D1),
  D1 = dia(Cs,_),
  name(Name,Cs),
  concat_atom([Scratch,'/',Name,'.',Ext],FileName),
  (
    exists_file(FileName),
    open(FileName,read,AstFile),
    read(AstFile,D2),
    close(AstFile),
    D1 == D2
  ;
    format(user,'Dumping file ~a ...\n',[FileName]),
    open(FileName,write,AstFile),
    format(AstFile,"~w.\n",[D1]),
    close(AstFile)
  ),
  diagrams(Scratch,Ext,Ds).


diagram(dia(Cs,Exp)) :-
  n(Cs),
  space,
  @ "=",
  nl,
  space,
  space,
  space,
  exp(Exp),
  nl, nl.


exp(nl) :- @ "\\\\", nl, space, space.
exp(concat(E1,E2)) :- exp(E1), space, exp(E2).
exp(t(Cs)) :- t(Cs).
exp(n(Cs)) :- n(Cs).
exp(or(epsilon,plus(E))) :- protect(E), @ "*".
exp(or(epsilon,E)) :- protect(E), @ "?".
exp(or(E1,E2)) :- @ "(", exp(E1), space, @ "|", space, orexp(E2), @ ")".
exp(p(E1,E2)) :- exp(E1), nl, @ "||", space, exp(E2).
exp(plus(E)) :- protect(E), @ "+".
exp(obrace) :- @ "{".
exp(cbrace) :- @ "}".
exp(footnote(Cs)) :- @ "@", format("~s",[Cs]).

orexp(or(E1,E2)) :- exp(E1), space, @ "|", space, orexp(E2).
orexp(E) :- exp(E).


n(Cs) :- format("~s",[Cs]).
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
   append(_,[--,AfterFile,EbnfFile,ModsFile,Scratch,Ext],Argv),
   see(AfterFile),
   read((Mods,Asts)),
   seen,
   format('Pretty-printing EBNF ~a ...\n',[EbnfFile]),
   tell(EbnfFile),
   diagrams(Scratch,Ext,Asts),
   told,
   tell(ModsFile),
   write(Mods), write('.'), nl,
   told,
   halt.
