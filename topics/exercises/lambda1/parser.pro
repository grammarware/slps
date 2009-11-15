% Program is a list of statements
expression(app(M,N)) --> keyword("app"), expression(M), keyword(":"), expression(N).
expression(lam(X,N)) --> keyword("lambda"), name(X), keyword("."), expression(N).
expression(var(N)) --> name(N).

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

% Identifiers are space-consuming letters
name(V) -->
 spaces,
 letter(H),
 { atom_chars(V,[H]) }.

% Bare letters
letters([H|T]) --> letter(H), letters(T).
letters([]) --> [].
letter(H,[H|T],T) :- H >= 0'a, H =< 0'z.

