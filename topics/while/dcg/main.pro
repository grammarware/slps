program(S) --> statements(S).

statements(slist(H,T)) -->
  statement(H), string(";"),
  statements(T).
statements(skip) --> [].

statement(assign(V,E)) -->
  varid(V),
  string(":="),
  expression(E).

expression(num(N)) --> num(N).

string([],X,X).
string([H|T1],[H|T2],X) :- string(T1,T2,X).

spaces --> [0' ], spaces.
spaces --> [].

num(N) -->
  spaces,
  digit(H), digits(T),
  { number_chars(N,[H|T]) }.

digits([H|T]) --> digit(H), digits(T).
digits([]) --> [].

digit(H,[H|T],T) :- H >= 0'0, H <= 0'9.
