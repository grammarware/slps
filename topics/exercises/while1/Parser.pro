program(S) --> statements(S).

statements(slist(H,T)) -->
 statement(H),
 string(";"),
 statements(T).
statements(S) --> statement(S).

statement(skip) --> string("skip").
statement(assign(identifier(V),E)) -->
 identifier(V),
 string(":="),
 expression(E).
statement(ifthenelse(E,S1,S2)) -->
 string("if"),
 expression(E),
 string("then"),
 statement(S1),
 string("else"),
 statement(S2).

expression(number(N)) --> number(N).
expression(true) --> string("true").
expression(false) --> string("false").

string([],X,X).
string([H|T1],[H|T2],X) :- string(T1,T2,X).

spaces --> [0' ], spaces. %'
spaces --> [].

number(N) -->
 spaces,
 digit(H), digits(T),
 { number_chars(N,[H|T]) }.

digits([H|T]) --> digit(H), digits(T).
digits([]) --> [].
digit(H,[H|T],T) :- H >= 0'0, H =< 0'9.

identifier(V) -->
 spaces,
 letter(H), letters(T),
 { atom_chars(V,[H|T]) }.

letters([H|T]) --> letter(H), letters(T).
letters([]) --> [].
letter(H,[H|T],T) :- H >= 0'a, H =< 0'z.
