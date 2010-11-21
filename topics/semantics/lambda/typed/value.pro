:- ['../applied/value.pro'].

% The untyped version is no longer to be used.

value(lam(X,A,T)) :- variable(X), type(A), term(T).

