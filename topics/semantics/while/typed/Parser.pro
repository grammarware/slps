%% Parser for While language
% Program is a list of statements
program(S) --> statements(S).

% Non-empty list
statements(slist(H,T)) -->
 statement(H),
 keyword(";"),
 statements(T).

% Empty list
statements(S) --> statement(S).

%% Statements
% Variable declaration
statement(decl(V)) -->
 keyword("var"),
 identifier(V).

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

% Loop statement
statement(while(E,S)) -->
 keyword("while"),
 bexpression(E),
 keyword("do"),
 statement(S).


%% Arithmetic expressions
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

% Same with subtraction
aexpression(sub(E1,E2)) -->
 keyword("("),
 aexpression(E1),
 keyword("-"),
 aexpression(E2),
 keyword(")").

% ...and multiplication
aexpression(mul(E1,E2)) -->
 keyword("("),
 aexpression(E1),
 keyword("*"),
 aexpression(E2),
 keyword(")").


%% Boolean expressions
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

% Negation is a boolean expression
bexpression(not(E)) -->
 keyword("¬"),
 bexpression(E).

% Disjunction of boolean expressions is also a boolean expression
bexpression(and(E1,E2)) -->
 keyword("("),
 bexpression(E1),
 keyword("^"),
 bexpression(E2),
 keyword(")").


%% Low-level details
% Dealing with layout
layout --> [0' ], layout.  %'  space
layout --> [0'	], layout. %'  tab
layout --> [10], layout.   %   newline
layout --> [].

% Keywords are space-consuming strings
keyword(X) -->
 layout,
 string(X).

% Bare strings
string([],X,X).
string([H|T1],[H|T2],X) :- string(T1,T2,X).

% Numbers are space-consuming digits
number(N) -->
 layout,
 digit(H), digits(T),
 { number_chars(N,[H|T]) }.

% Bare digits
digits([H|T]) --> digit(H), digits(T).
digits([]) --> [].
digit(H,[H|T],T) :- H >= 0'0, H =< 0'9.

% Identifiers are space-consuming letters
identifier(V) -->
 layout,
 letter(H), letters(T),
 { atom_chars(V,[H|T]) }.

% Bare letters
letters([H|T]) --> letter(H), letters(T).
letters([]) --> [].
letter(H,[H|T],T) :- H >= 0'a, H =< 0'z.

