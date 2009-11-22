% See slide 186

term(var(X)) :- atom(X).
term(app(T1,T2)) :- term(T1), term(T2).
term(V) :- val(V).

val(lam(X,XT,T)) :- atom(X), type(XT), term(T).
val(true).
val(false).

type(bool).
type(maps(T1,T2)) :- type(T1), type(T2).

