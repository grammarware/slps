% See slide 174

term(var(X)) :- variable(X).
term(app(T1,T2)) :- term(T1), term(T2).
term(lam(X,T)) :- variable(X), term(T).

value(lam(X,T)) :- variable(X), term(T).

variable(a).
variable(f).
variable(g).
variable(x).
variable(y).
variable(z).
