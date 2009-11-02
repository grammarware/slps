% Program is a list of statements
program(S) --> statements(S).

% Non-empty list
statements(slist(H,T)) -->
 statement(H),
 keyword(";"),
 statements(T).

% Empty list
statements(S) --> statement(S).

% Statements
% Skip statement
statement(skip) -->
 keyword("skip").

% Assign statement
statement(assign(identifier(V),E)) -->
 identifier(V),
 keyword(":="),
 aexpression(E).

% Conditional statement
statement(ifthenelse(E,S1,S2)) -->
 keyword("if"),
 bexpression(E),
 keyword("then"),
 statement(S1),
 keyword("else"),
 statement(S2).

% Expressions
% Number is a an arithmetic expression
aexpression(number(N)) --> number(N).

% Variable reference is a an arithmetic expression
aexpression(identifier(V)) --> identifier(V).

% A sum of arithmetic expressions is also an arithmetic expression
aexpression(add(E1,E2)) -->
 keyword("("),
 aexpression(E1),
 keyword("+"),
 aexpression(E2),
 keyword(")").

% Boolean primitives are boolean expressions
bexpression(true) --> keyword("true").
bexpression(false) --> keyword("false").

% Comparison is a boolean expression: equality
bexpression(equals(E1,E2)) -->
 aexpression(E1),
 keyword("="),
 aexpression(E2).

% Comparison is a boolean expression: less than or equal to
bexpression(lte(E1,E2)) -->
 aexpression(E1),
 keyword("≤"),
 aexpression(E2).

% Dealing with spaces
spaces --> [0' ], spaces. %'
spaces --> [].

% Keywords are space-consuming strings
keyword(X) -->
 spaces,
 string(X).

% Bare strings
string([],X,X).
string([H|T1],[H|T2],X) :- string(T1,T2,X).

% Numbers are space-consuming digits
number(N) -->
 spaces,
 digit(H), digits(T),
 { number_chars(N,[H|T]) }.

% Bare digits
digits([H|T]) --> digit(H), digits(T).
digits([]) --> [].
digit(H,[H|T],T) :- H >= 0'0, H =< 0'9.

% Identifiers are space-consuming letters
identifier(V) -->
 spaces,
 letter(H), letters(T),
 { atom_chars(V,[H|T]) }.

% Bare letters
letters([H|T]) --> letter(H), letters(T).
letters([]) --> [].
letter(H,[H|T],T) :- H >= 0'a, H =< 0'z.

