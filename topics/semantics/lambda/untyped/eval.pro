:- ensure_loaded('../../shared/manysteps.pro').

eval(app(T1,T2),app(T3,T2)) :-
  eval(T1,T3).

eval(app(V,T1),app(V,T2)) :-
  value(V),
  eval(T1,T2).

eval(app(lam(X,T1),V),T2) :-
  value(V),
  substitute(V,X,T1,T2).

