:- ['../applied/term.pro'].

% The untyped version is no longer part of surface syntax.

term(lam(X,A,T)) :- variable(X), type(A), term(T).


% We add the fix construct.

term(fix(T)) :- term(T).


% Adding syntax for types

type(bool).
type(nat).
type(fun(A1,A2)) :- type(A1), type(A2).

