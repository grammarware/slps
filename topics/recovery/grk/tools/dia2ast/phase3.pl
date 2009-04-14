% A diagram consisting at least of one segment
diagram(Cs,[Ast|Asts]) -->
  segment(start,End,_,_,Ymin,_,_,Ast),
  continuation(End,Ymin,Asts),
  boxed(Cs).

boxed(Cs) -->
  select((hline,X1,X2,_,_,Ytop)),
  select((nonterminal(Cs),X3,X4,_,_,Ytop)),
  select((hline,X5,X6,_,_,Ytop)),
  select((hline,X1,X6,_,_,Ybottom)),
  select((vline,_,X1,Ybottom,Ytop,_)),
  select((vline,X6,_,Ybottom,Ytop,_)),
  { X3 >= X2,
    X5 >= X4
  }.

boxed([]) --> [].



% Continuation segments in a diagram
continuation(continue,Ysofar,[Ast|Asts]) -->
  segment(continue,End,_,_,Ymin,Ymax,_,Ast),
  continuation(End,Ymin,Asts),
  { Ysofar > Ymax },
  !.

continuation(finish,_,[]) --> !.



% Horizontal segmemts in a diagram
segment(Begin,End,Xmin,Xmax,Ymin,Ymax,Ycenter,Ast) -->
  { ( Begin = start
    ; Begin = continue
    )
  },
  select((Begin,Xmin,X1,YstartMin,YstartMax,Ycenter)),
  list(X1,X2,YlistMin,YlistMax,Ycenter,Ast),
  { ( End = finish
    ; End = continue
    )
  },
  select((End,X2,Xmax,YfinishMin,YfinishMax,Ycenter)),
  { Ymin is min(YstartMin,min(YlistMin,YfinishMin)) },
  { Ymax is max(YstartMax,max(YlistMax,YfinishMax)) },
  !.



% List-like structures
list(Xmin,Xmax,Ymin,Ymax,Ycenter,concat(Ast1,Ast2)) -->
  select((hline,Xmin,X1,YhlineMin,YhlineMax,Ycenter)),
  element(X1,X2,YelementMin,YelementMax,Ycenter,Ast1),
  list(X2,Xmax,YlistMin,YlistMax,Ycenter,Ast2),
  { Ymin is min(YhlineMin,min(YelementMin,YlistMin)) },
  { Ymax is max(YhlineMax,max(YelementMax,YlistMax)) },
  !.

% Fine-tuning: accept a period right after another element
list(Xmin,Xmax,Ymin,Ymax,Ycenter,concat(t([C]),Ast2)) -->
  select((terminal([C]),Xmin,X1,YperiodMin,YperiodMax,Ycenter)),
  { isSpecial(C) }, 
  list(X1,Xmax,YlistMin,YlistMax,Ycenter,Ast2),
  { Ymin is min(YperiodMin,YlistMin) },
  { Ymax is max(YperiodMax,YlistMax) },
  !.

list(Xmin,Xmax,Ymin,Ymax,Ycenter,epsilon) -->
  select((hline,Xmin,Xmax,Ymin,Ymax,Ycenter)),
  !.



% Terminals
element(Xmin,Xmax,Ymin,Ymax,Ycenter,t(Chars)) -->
  select((terminal(Chars),Xmin,Xmax,Ymin,Ymax,Ycenter)),
  !.

% Nonterminals
element(Xmin,Xmax,Ymin,Ymax,Ycenter,n(Chars)) -->
  select((nonterminal(Chars),Xmin,Xmax,Ymin,Ymax,Ycenter)),
  !.

% Footnotes
element(Xmin,Xmax,Ymin,Ymax,Ycenter,footnote(Cs)) -->
  select((footnote(Cs),Xmin,Xmax,Ymin,Ymax,Ycenter)),
  !.

% IBM extensions opening brace
element(Xmin,Xmax,Ymin,Ymax,Ycenter,obrace) -->
  select((obrace,Xmin,Xmax,Ymin,Ymax,Ycenter)),
  !.

% IBM extensions closing brace
element(Xmin,Xmax,Ymin,Ymax,Ycenter,cbrace) -->
  select((cbrace,Xmin,Xmax,Ymin,Ymax,Ycenter)),
  !.

% Lists as the contribute to some constructs
element(Xmin,Xmax,Ymin,Ymax,Ycenter,Ast0) -->
  list(Xmin,XmaxList,YminList,YmaxList,Ycenter,Ast1),
  complete(Xmin,XmaxList,Xmax,YminList,Ymin,YmaxList,Ymax,Ycenter,Ast1,Ast0),
  !.

% Plus operator
complete(Xmin,XmaxList,Xmax,YminList,YminList,YmaxList,Ymax,Ycenter,Ast,plus(Ast)) -->
  select((vline,XmaxList,Xmax,Ycenter,Yedge,undefined)),
  select((arrow,Xmin,XmaxList,Yarrow,Ymax,Yedge)),
  { Yarrow >= YmaxList }.

% Alternatives
complete(Xmin,XmaxList,XmaxList,YminList,Ymin,YmaxList,YmaxList,Ycenter,Ast1,or(Ast1,Ast2)) -->
  stack(Xmin,XmaxList,Ymin,YmaxStack,Ycenter,Ast2),
  { YminList >= YmaxStack },
  !.



% Helper for stacked alternatives
stack(Xmin,Xmax,Ymin,Ymax,Yconnect,or(Ast1,Ast2)) -->
  push(Xmin,Xmax,YminPush,Ymax,Ycenter,Yconnect,Ast1),
  stack(Xmin,Xmax,Ymin,YmaxStack,Ycenter,Ast2),
  { YminPush >= YmaxStack },
  !.

stack(Xmin,Xmax,Ymin,Ymax,Yconnect,Ast) -->
  push(Xmin,Xmax,Ymin,Ymax,_,Yconnect,Ast),
  !.



% One level of stacking
push(Xmin,Xmax,Ymin,Ymax,Ycenter,Yconnect,Ast) -->
  select((vline,Xmin,X1,Ycenter,Yconnect,undefined)),
  list(X1,X2,Ymin,Ymax,Ycenter,Ast),
  select((vline,X2,Xmax,Ycenter,Yconnect,undefined)),
  { Yconnect >= Ycenter }.
