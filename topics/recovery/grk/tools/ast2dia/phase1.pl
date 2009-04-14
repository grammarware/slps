diagram(Mode,Cs,Asts,Ts) :-
  Xz = 5,
  segments(Mode,Xz,0,Conts,Asts,Ts1),
  ymax(Ts1,0,Ymax0),
  Ymax1 is Ymax0 + 2,
  ( Cs == [],
    afirst(First,0,Ymax1,Xmax0)
  ; 
    \+ Cs == [],
    first(Cs,First,0,Ymax1,Xmax0)
  ),
  Ts0 = [First],
  xmax(Ts1,Xmax0,Xmax1),
  aligned(Conts,Xmax1,Ts2),
  append(Ts0,Ts1,Ts3),
  append(Ts3,Ts2,Ts).


mode2begin(concat,start).
mode2begin(p,pcontinue).

mode2cont(concat,continue).
mode2cont(p,pcontinue).


% A one-liner diagram
segments(Mode,Xz,Y0,[alignfinish(Mode,Y0,X0)],[Ast],Ts) :- 
  mode2begin(Mode,Begin),
  segment(Begin,Xz,Y0,X0,Ast,Ts).

% A diagram with continuation lines  
segments(Mode,Xz,Y0,[aligncontinue(Mode,Y0,X0)|Conts],[Ast1|[Ast2|Asts]],Ts) :-
  mode2begin(Mode,Begin),
  segment(Begin,Xz,Y0,X0,Ast1,Ts1),
  ymin(Ts1,Y0,Y1),
  lcom(Ast2,Y1,Y2),
  below(Y2,Y3),
  continuation(Mode,Xz,Y3,Conts,[Ast2|Asts],Ts2),
  append(Ts1,Ts2,Ts).



% The last continuation line in a diagram
continuation(Mode,Xz,Y0,[alignfinish(Mode,Y0,X0)],[Ast],Ts) :-
  mode2cont(Mode,Cont),
  segment(Cont,Xz,Y0,X0,Ast,Ts).

% A continuation line
continuation(Mode,Xz,Y0,[aligncontinue(Mode,Y0,X0)|Conts],[Ast1|[Ast2|Asts]],Ts) :-
  mode2cont(Mode,Cont),
  segment(Cont,Xz,Y0,X0,Ast1,Ts1),
  ymin(Ts1,Y0,Y1),
  lcom(Ast2,Y1,Y2),
  below(Y2,Y3),
  continuation(Mode,Xz,Y3,Conts,[Ast2|Asts],Ts2),
  append(Ts1,Ts2,Ts).  



% Align the finish sequence in a diagram
alignfinish(Mode,Y0,X0,Xmax,[T1,T2]) :-
  Inc is Xmax - X0,
  hline(Inc,T1,X0,Y0,X1),
  finish(Mode,T2,X1,Y0,_).



% Align the continue sequence in a diagram
aligncontinue(Mode,Y0,X0,Xmax,[T1,T2]) :-
  Inc is Xmax - X0,
  hline(Inc,T1,X0,Y0,X1),
  rcontinue(Mode,T2,X1,Y0,_).



% Horizontal composition of start, list, and finish
segment(Begin,Xz,Y,X2,Ast,[T1|Ts]) :-
  ( Begin == start, start(T1,Xz,Y,X1)
  ; Begin == continue, lcontinue(T1,Xz,Y,X1)
  ; Begin == pcontinue, pcontinue(T1,Xz,Y,X1)
  ),
  list(Ast,Ts,X1,Y,X2).



% Trivial list
list(epsilon,[T],X0,Y0,X1) :-
  hline(2,T,X0,Y0,X1).

% Nested line breaks
list(concat(nl,Ast),Ts,X0,Y0,X1) :- 
 list(Ast,Ts,X0,Y0,X1).

% Horizontal composition for a non-trivial list
list(concat(Ast1,Ast2),[T|Ts],X0,Y0,X3) :-
  hline(2,T,X0,Y0,X1),
  element(Ast1,Ts1,X1,Y0,X2),
  list(Ast2,Ts2,X2,Y0,X3),
  append(Ts1,Ts2,Ts).

% Singleton lists
list(Ast,Ts,X0,Y0,X1) :- 
  \+ Ast = epsilon,
  \+ Ast = concat(_,_),
  list(concat(Ast,epsilon),Ts,X0,Y0,X1).



% Terminals as elements
element(t(Cs),[T],X0,Y0,X1) :-
  terminal(Cs,T,X0,Y0,X1).

% Nonterminals as elements
element(n(Cs),[T],X0,Y0,X1) :-
  nonterminal(Cs,T,X0,Y0,X1).

% Footnote as elements
element(footnote(Cs),[T],X0,Y0,X1) :-
  footnote(Cs,T,X0,Y0,X1).

% IBM extensions opening brace
element(obrace,[T],X0,Y0,X1) :-
  obrace(T,X0,Y0,X1).

% IBM extensions closing brace
element(cbrace,[T],X0,Y0,X1) :-
  cbrace(T,X0,Y0,X1).

% Iteration
element(plus(Ast),[T1,T2|Ts],X0,Y0,X2) :-
  list(Ast,Ts,X0,Y0,X1),
  ymax(Ts,Y0,Y1),
  vline(Y1,T1,X1,Y0,X2),
  above(Y1,Y2),
  arrow(T2,X0,Y2,X1).

% Stacked alternatives
element(or(Ast1,Ast2),[T1|Ts],X0,Y,Xalign) :-
  hline(1,T1,X0,Y,X1),
  list(Ast1,Ts1,X1,Y,X2),
  ymin(Ts1,Y,Y1),
  lcom(Ast2,Y1,Y2),
  stack(Y,Conts,Ast2,Ts2,X0,Y2,X3),
  Xmax is max(X2,X3),
  append(Ts1,Ts2,Ts3),
  aligned([aligntop(Y,X2)|Conts],Xmax,Ts4),
  xmax(Ts4,Xmax,Xalign),
  append(Ts3,Ts4,Ts).



% Stacks as non-empty lists of alternatives
stack(Y0,[alignpush(Y0,Y,X1)|Conts],or(Ast1,Ast2),Ts,X0,Y,Xmax) :-
  push(Y0,Ast1,Ts1,X0,Y,X1), 
  ymin(Ts1,Y,Y1),
  lcom(Ast2,Y1,Y2),
  stack(Y,Conts,Ast2,Ts2,X0,Y2,X2),
  Xmax is max(X1,X2),
  append(Ts1,Ts2,Ts).

stack(Y0,[alignpush(Y0,Y,X1)],Ast,Ts,X0,Y,X1) :-
  push(Y0,Ast,Ts,X0,Y,X1).



% Stacking level
push(Y0,Ast,[T1|Ts],X0,Y,X2) :-
  vline(Y0,T1,X0,Y,X1),
  list(Ast,Ts,X1,Y,X2).



% Align the top level of a stack
aligntop(Y0,X0,Xmax,[T]) :-
  Inc is Xmax - X0 + 1,
  hline(Inc,T,X0,Y0,_).



% Align the non-top levels of a stack
alignpush(Y,Y0,X0,Xmax,[T1,T2]) :-
  Inc is Xmax - X0,
  hline(Inc,T1,X0,Y0,X1),
  vline(Y,T2,X1,Y0,_).



% "Lower Center Of Meaning" to allow space needed for plus arrows
lcom(concat(Ast1,Ast2),Y0,Y) :-  
  lcom(Ast1,Y0,Y1),
  lcom(Ast2,Y0,Y2),
  Y is min(Y1,Y2).

lcom(plus(Ast),Y0,Y2) :-
  below(Y0,Y1),
  lcom(Ast,Y1,Y2).

lcom(or(Ast,_),Y0,Y1) :- 
  lcom(Ast,Y0,Y1).

lcom(noempty(Ast),Y0,Y2) :-  
  below(Y0,Y1),
  lcom(Ast,Y1,Y2).

lcom(_,Y,Y).



% Generate a start token
start((start,X0,X1,Y1,Y0),X0,Y0,X1) :-
  X1 is X0 + 2,
  Y1 is Y0 - 1.



% Generate a continue token
lcontinue((lcontinue,X0,X1,Y1,Y0),X0,Y0,X1) :-
  X1 is X0 + 2,
  Y1 is Y0 - 1.
  
rcontinue(Mode,(Cont,X0,X1,Y1,Y0),X0,Y0,X1) :-
  ( Mode == concat, Cont = rcontinue
  ; Mode == p, Cont = pcontinue
  ),
  X1 is X0 + 2,
  Y1 is Y0 - 1.

pcontinue((pcontinue,X0,X1,Y1,Y0),X0,Y0,X1) :-
  X1 is X0 + 2,
  Y1 is Y0 - 1.



% Generate a finish token
finish(Mode,(Finish,X0,X1,Y1,Y0),X0,Y0,X1) :-
  ( Mode == concat, Finish = finish
  ; Mode == p, Finish = pcontinue
  ),
  X1 is X0 + 2,
  Y1 is Y0 - 1.



% Generate a hline token of variable length
hline(Inc,(hline,X0,X1,Y1,Y0),X0,Y0,X1) :-
  X1 is X0 + Inc,
  Y1 is Y0 - 1.



% Generate a vline of unit length
vline(Y,(vline,X0,X1,Y1,Y),X0,Y0,X1) :-
  X1 is X0 + 1,
  Y1 is Y0 - 1.



% Generate a terminal token
terminal(Cs,(terminal(Cs),X0,X1,Y1,Y0),X0,Y0,X1) :-
  length(Cs,Inc),
  X1 is X0 + Inc,
  Y1 is Y0 - 1.



% Generate a nonterminal token
nonterminal(Cs,(nonterminal(Cs),X0,X1,Y1,Y0),X0,Y0,X1) :-
  length(Cs,Inc),
  X1 is X0 + Inc,
  Y1 is Y0 - 1.


% Generate a footnote token
footnote(Cs,(footnote(Cs),X0,X1,Y1,Y0),X0,Y0,X1) :-
  length(Cs,Len),
  X1 is X0 + Len + 1,
  Y1 is Y0 - 1.



% Generate an obrace token
obrace((obrace,X0,X1,Y1,Y0),X0,Y0,X1) :-
  X1 is X0 + 1,
  Y1 is Y0 - 1.



% Generate a cbrace token
cbrace((cbrace,X0,X1,Y1,Y0),X0,Y0,X1) :-
  X1 is X0 + 1,
  Y1 is Y0 - 1.



% Generate an arrow token
arrow((arrow,X0,X1,Y1,Y0),X0,Y0,X1) :-
  Y1 is Y0 - 1.



% Generate a first line
first(Cs,(first(Cs),X0,X1,Y1,Y0),X0,Y0,X1) :-
  Y1 is Y0 - 1,
  length(Cs,Len),
  X1 is X0 + Len + 9.



% Generate an anonymous first line
afirst((afirst,X0,X1,Y1,Y0),X0,Y0,X1) :-
  Y1 is Y0 - 1,
  X1 is X0 + 6.



% To go upwards and downwards to allow space for arrows
above(Y0,Y1) :- Y1 is Y0 + 1.
below(Y0,Y1) :- Y1 is Y0 - 1.



% Minimal occupied Y coordinate 
ymin(Ts,Y0,Y1) :- member((_,_,_,Ymin,_),Ts), Ymin < Y0, !, ymin(Ts,Ymin,Y1).
ymin(_,Y,Y).



% Maximal occupied Y coordinate 
ymax(Ts,Y0,Y1) :- member((_,_,_,_,Ymax),Ts), Ymax > Y0, !, ymax(Ts,Ymax,Y1).
ymax(_,Y,Y).



% Maximal occupied X coordinate 
xmax(Ts,X0,X1) :- member((_,_,Xmax,_,_),Ts), Xmax > X0, !, xmax(Ts,Xmax,X1).
xmax(_,X,X).



% A parameterized predicate for alignment
aligned([],_,[]).
aligned([Cont|Conts],Xmax,Ts) :-
  call(Cont,Xmax,Ts1),
  aligned(Conts,Xmax,Ts2),
  append(Ts1,Ts2,Ts).
