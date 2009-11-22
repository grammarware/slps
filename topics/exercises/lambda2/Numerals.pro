%
% See slide 168: Church numerals
%

:- ['Syntax.pro'].
:- ['Evaluation.pro'].

evalcn(lam(s,lam(z,var(z))),0).

evalcn(lam(s,lam(z,app(var(s),C))),N) :-
 evalcn(lam(s,lam(z,C)),N1),
 N is N1 + 1.
