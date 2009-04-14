% Normalisation of EBNF expressions
normalise(Ast1,Ast2) :-
  innermost(normstep,Ast1,Ast2).

% epsilon as left and right unit of concat
normstep(concat(X,epsilon),X).
normstep(concat(epsilon,X),X).

% Right-associative conjunctions
normstep(concat(concat(X,Y),Z),concat(X,concat(Y,Z))).

% Right-associative disjunctions
normstep(or(or(X,Y),Z),or(X,or(Y,Z))).

% Right-associative disjunctions
normstep(p(p(X,Y),Z),p(X,p(Y,Z))).

% Prefer optional arrow over iterated optional
normstep(plus(or(epsilon,X)),or(epsilon,plus(X))).

% Elimination of line-breaks for empty lines
normstep(concat(X,nl),X).
normstep(concat(nl,concat(nl,X)),concat(nl,X)).



% Turn a single expression into a list of expressions
toList(concat(X,Ast),[X|Asts]) :- toList(Ast,Asts).
toList(X,[X]) :- \+ X = concat(_,_).



% Turn a list from expressions into a single expression
fromList([X],X).
fromList([X,Y|Z],concat(X,V)) :- fromList([Y|Z],V).



% Turn list of expressions into a single expression with line breaks 
list2nl([Ast],Ast).
list2nl([Ast1|Asts],concat(Ast1,concat(nl,Ast2))) :- list2nl(Asts,Ast2).



% Turn a single expression with line breaks into a list of expressions
nl2list(concat(nl,Ast),[epsilon|Asts]) :- nl2list(Ast,Asts).
nl2list(concat(X,Ast1),[concat(X,Ast2)|Asts]) :- nl2list(Ast1,[Ast2|Asts]).
nl2list(X,[X]) :- \+ X = concat(_,_), \+ X = p(_,_).



%
% Turn a list of expressions into a permutation phrase
% Check that at least one line break occured
%
nl2p(X,Z) :-
  nl2p(fail,G,epsilon,X,Y), G, normalise(Y,Z).

nl2p(_,true,X,concat(nl,Y),p(X,Z)) :-
  nl2p(_,_,epsilon,Y,Z).

nl2p(G1,G2,X,concat(Y,Z),V) :-
  \+ Y = nl, nl2p(G1,G2,concat(X,Y),Z,V).

nl2p(G,G,X,Y,concat(X,Y)) :-
  \+ Y = concat(_,_).



% Turn a permutation phrase into a list of expressions
p2list(p(X,Y),[X|Asts]) :- p2list(Y,Asts).
p2list(Ast,[Ast]) :- \+ Ast = p(_,_).
