:-
   load_files(['../../lib/io.pl',
               '../../lib/traversal.pl',
               './normalise.pl',
               './transform.pl'
              ],
              [if(not_loaded),silent(true)]).

script([(Com,T)|Ts]) --> 
  comment(Com),
  trafo(T),
  @(";"),
  !,
  script(Ts).

script([]) --> layout.

trafo(rename(Cs0,Cs1)) -->
   @("Rename"),
   n(Cs0),
   @("To"),
   n(Cs1).

trafo(reject(Cs)) -->
   @("Reject"),
   n(Cs).

trafo(add(Ast,Whereto)) -->
   @("Add"),
   rule(Ast),
   whereto(Whereto).

trafo(replace(E1,E2,In)) -->
   @("Replace"),
   exp(E1),
   @("By"),
   oexp(E2),
   in(In).

trafo(extract(Dia,Cs)) -->
   @("Extract"),
   rule(Dia),
   @("From"),
   n(Cs).

trafo(fold(Cs1,Cs2)) -->
   @("Fold"),
   n(Cs1),
   @("In"),
   n(Cs2).

trafo(unfold(Cs1,Cs2)) -->
   @("Unfold"),
   n(Cs1),
   @("In"),
   n(Cs2).

trafo(permute(Cs)) -->
   @("Permute"),
   n(Cs).

trafo(nonempty(Cs)) -->
   @("Nonempty"),
   n(Cs).

trafo(extend(Cs,E)) -->
   @("Extend"),
   n(Cs),
   @("By"),
   exp(E).


whereto(eos(Sec)) -->
   @("To"),
   section(Sec).

whereto(after(Cs)) -->
   @("After"),
   n(Cs).

whereto(before(Cs)) -->
   @("Before"),
   n(Cs).

whereto(anywhere) --> [].

in(n(Cs)) -->
   @("In"),
   n(Cs).

in(everywhere) --> [].



@(K,L1,L3) :-
  append(K,L2,L1),
  ( L2 = [C|_],
    \+ alphanum(C)
  ;
    reverse(K,[C|_]),
    \+ alphanum(C)
  ),
  !,
  layout(L2,L3).

comment(Cs,L1,L3) :-
  Prefix = [0'%|Cs],
  append(Prefix,L2,L1),
  L2 = [10|_],
  \+ member(10,Cs),
  !,
  layout(L2,L3).

section(Cs,L1,L3) :-
  append(Cs,L2,L1),
  \+ (member(X,Cs), \+ (digit(X); X=0'.)),
  member(Y,Cs), digit(Y),
  member(Z,Cs), Z=0'.,
  L2 = [C|_],
  \+ (digit(C); C =0'.),
  !,
  layout(L2,L3).

n(Cs,L1,L3) :-
  append(Cs,L2,L1),
  Cs = [C1|_],
  isLower(C1),
  \+ (member(X,Cs), \+ alphanum(X)),
  L2 = [C|_],
  \+ alphanum(C),
  !,
  layout(L2,L3).

t(Cs,L1,L3) :-
  append([0'"|Cs],[0'"|L2],L1),
  \+ member(0'",Cs),
  !,
  layout(L2,L3).

rule(dia(Cs,Exp)) -->
  n(Cs),
  @("="),
  exp(Exp).


oexp(E)       --> exp(E).
oexp(epsilon) --> [].


exp(E2) --> sexp(E1), orrest(E1,E2).
orrest(E1,or(E1,E3)) --> @("|"), sexp(E2), orrest(E2,E3).
orrest(E1,p(E1,E3)) --> @("||"), sexp(E2), orrest(E2,E3).
orrest(E,E) --> [].


sexp(E2) --> uexp(E1), srest(E1,E2).
srest(E1,concat(E1,E3)) --> uexp(E2), srest(E2,E3).
srest(E,E) --> [].


uexp(E2) --> bexp(E1), urest(E1,E2).
urest(E,test(E))             --> @("#").
urest(E,or(epsilon,E))       --> @("?").
urest(E,plus(E))             --> @("+").
urest(E,or(epsilon,plus(E))) --> @("*").
urest(E,E)                   --> [].


bexp(cut)    --> @("!").
bexp(obrace) --> @("{").
bexp(cbrace) --> @("}").
bexp(nl)     --> @("\\\\").
bexp(t(Cs))  --> t(Cs).
bexp(n(Cs))  --> n(Cs).
bexp(E)      --> @("("), exp(E), @(")").
bexp(footnote(Cs)) --> footnote(Cs). 


layout([C|Cs1],Cs2) :-
  member(C,[10,32]),
  layout(Cs1,Cs2).

layout([0'/,0'*|Cs1],Cs3) :-
  append(_,[0'*,0'/|Cs2],Cs1),
  !,
  layout(Cs2,Cs3).  

layout(X,X).

footnote([C],[0'@,C|Cs1],Cs2) :-
  digit(C),
  layout(Cs1,Cs2).

alphanum(C) :- C >= 0'A, C =< 0'Z. 
alphanum(C) :- C >= 0'a, C =< 0'z. 
alphanum(C) :- digit(C).
alphanum(0'-). 
digit(C) :- C >= 0'0, C =< 0'9. 



interpret([],X,X).

interpret([T|Ts],X,Z) :-
  interpret(T,X,Y),
  interpret(Ts,Y,Z).


interpret((Com,rename(Cs1,Cs2)),(Mods1,Asts1),(Mods2,Asts2)) :-
  ( append(Asts3,[dia(Cs1,E)|Asts4],Asts1),
    !,
    append(Asts3,[dia(Cs2,E)|Asts4],Asts5),
    [rename(Com,Cs1,Cs2)|Mods1] = Mods3
  ;
    Asts1 = Asts5,
    Mods1 = Mods3
  ),
  replaceEverywhere(Com,n(Cs1),n(Cs2),Asts5,Asts2,Mods4),
  \+ Asts1 == Asts2,
  append(Mods3,Mods4,Mods2).

interpret((Com,reject(Cs)),(Mods,Asts1),([Mod|Mods],Asts2)) :-
  Mod = del(Com,Cs),
  append(Asts3,[dia(Cs,_)|Asts4],Asts1),
  append(Asts3,Asts4,Asts2).

interpret((Com,add(Dia,eos(Sec))),(Mods,Asts),([Mod|Mods],[Dia|Asts])) :-
  Dia = dia(Cs,_),
  Mod = add(Com,Cs,eos(Sec)),
  \+ append(_,[dia(Cs,_)|_],Asts).

interpret((Com,add(Dia,after(Cs1))),(Mods,Asts),([Mod|Mods],[Dia|Asts])) :-
  Dia = dia(Cs2,_),
  Mod = add(Com,Cs2,after(Cs1)),
  append(_,[dia(Cs1,_)|_],Asts),
  \+ append(_,[dia(Cs2,_)|_],Asts).

interpret((Com,add(Dia,before(Cs1))),(Mods,Asts),([Mod|Mods],[Dia|Asts])) :-
  Dia = dia(Cs2,_),
  Mod = add(Com,Cs2,before(Cs1)),
  append(_,[dia(Cs1,_)|_],Asts),
  \+ append(_,[dia(Cs2,_)|_],Asts).

interpret((Com,add(Dia,anywhere)),(Mods,Asts),([Mod|Mods],[Dia|Asts])) :-
  Dia = dia(Cs2,_),
  Mod = add(Com,Cs2,anywhere),
  \+ append(_,[dia(Cs2,_)|_],Asts).

interpret((Com,replace(E1,E2,n(Cs))),(Mods,Asts1),([Mod|Mods],Asts2)) :-
  Mod = replace(Com,Cs),
  replace(Cs,E1,E2,Asts1,Asts2).

interpret((Com,replace(E1,E2,everywhere)),(Mods1,Asts1),(Mods3,Asts2)) :-
  replaceEverywhere(Com,E1,E2,Asts1,Asts2,Mods2),
  \+ Asts1 == Asts2,
  append(Mods1,Mods2,Mods3).

interpret((Com,extract(Ast,Cs1)),
          (Mods,Asts1),
          ([Mod|Mods],[Ast|Asts2])) :-
  Ast = dia(Cs2,E1),
  Mod = extract(Com,Cs2,Cs1),
  \+ append(_,[dia(Cs2,_)|_],Asts1),
  replace(Cs1,E1,n(Cs2),Asts1,Asts2).

interpret((Com,fold(Cs2,Cs1)),
          (Mods,Asts1),
          ([Mod|Mods],Asts2)) :-
  append(_,[dia(Cs2,E1)|_],Asts1),
  Mod = fold(Com,Cs1,Cs2),
  replace(Cs1,E1,n(Cs2),Asts1,Asts2).

interpret((Com,unfold(Cs2,Cs1)),
          (Mods,Asts1),
          ([Mod|Mods],Asts2)) :-
  append(_,[dia(Cs2,E1)|_],Asts1),
  Mod = unfold(Com,Cs1,Cs2),
  replace(Cs1,n(Cs2),E1,Asts1,Asts2).

interpret((Com,permute(Cs)),(Mods,Asts1),([Mod|Mods],Asts2)) :-
  Mod = replace(Com,Cs),
  append(Asts3,[dia(Cs,E1)|Asts4],Asts1),
  append(Asts3,[dia(Cs,E2)|Asts4],Asts2),
  nl2p(E1,E2).

interpret((Com,nonempty(Cs)),(Mods,Asts1),([Mod|Mods],Asts2)) :-
  Mod = replace(Com,Cs),
  append(Asts3,[dia(Cs,E1)|Asts4],Asts1),
  append(Asts3,[dia(Cs,E2)|Asts4],Asts2),
  nonempty(E1,E2).

interpret((Com,extend(Cs,E)),(Mods,Asts1),([Mod|Mods],Asts2)) :-
  Mod = replace(Com,Cs),
  replace(Cs,E1,or(E1,E),Asts1,Asts2).

interpret((Com,T),X,X) :-
  T =.. [Op0|Args],
  ( Op0 == add,
    Args = [_,Arg2],
    Arg2 =.. [Where|_],
    Op1 = add(Where)
  ;
    Op0 == replace,
    Args = [_,_,Arg3],
    Arg3 =.. [Where|_],
    Op1 = replace(Where)  
  ;
    \+ Op0 == add,
    \+ Op0 == replace,
    Op1 = Op0 
  ),
  hint(Op1,Hint),
  format('Transformation failed.\n',[]),
  format('Comment:  % ~s\n',[Com]),
  format('Operator: ~w\n',[Op1]),
  format('Hint:     ~s\n',[Hint]),
  halt(1).


hint(rename,"old name must be in use; new name must be fresh.").
hint(reject,"name not found.").
hint(add(anywhere),"added nonterminal must be undefined.").
hint(add(eos),"added nonterminal must be undefined; section must exists.").
hint(add(before),"added name must be undefined; ""before"" target must exist.").
hint(add(after),"added name must be undefined; ""after"" target must exist.").
hint(replace(n),"phrase to replace did not match for given name.").
hint(replace(everywhere),"phrase to replace did not match anywhere.").
hint(extract,"phrase to replace must match; name to extract must be fresh.").
hint(fold,"involved names must be defined; phrase to be folded must occur.").
hint(unfold,"involved names must be defined.").
hint(permute,"name must be defined and admit a permutation phrase.").
hint(extend,"name to be extended must be defined.").
hint(_,"AI failed; no specific hint available.").


:- 
   unix(argv(Argv)),
   append(_,[--,FstFile,InFile,OutFile],Argv),
   see(InFile),
   format('Parsing script ~a ...\n',[FstFile]),
   read(MAs1),
   seen,
   see(FstFile),
   getChars(Cs1),
   seen,
   !,
   ( 
     layout(Cs1,Cs2),
     script(Ts,Cs2,[]),
     format('Applying transformations ...\n',[]),
     !,
     interpret(Ts,MAs1,MAs2),
     tell(OutFile),
     write(MAs2),
     write('.'),
     nl,
     told,
     halt
   ;
     format('Parsing script ~a failed.\n',[FstFile]),
     halt(1)
   ).
