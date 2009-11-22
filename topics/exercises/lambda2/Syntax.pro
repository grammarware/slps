% See slide 174

term(var(X)) :- variable(X).
term(app(T1,T2)) :- term(T1), term(T2).
term(lam(X,T)) :- variable(X), term(T).

value(lam(X,T)) :- variable(X), term(T).
value(var(X)) :- variable(X). % pragmatic extension to deal with open terms

variable(X) :- atom(X).
