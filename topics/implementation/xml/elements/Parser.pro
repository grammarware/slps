% A root tree is a regular tree with an opening and closing tags and a forest in between
tree(tree(N,L)) -->
 spaces,
 string("<"),
 tag(N),
 string(">"),
 trees(L),
 string("</"),
 tag(N),
 string(">"),
 spaces.

% A non-empty forest
trees([H|T]) -->
 tree(H),
 trees(T).

% An empty forest
trees([]) --> [].

% Bare strings
string([],X,X).
string([H|T1],[H|T2],X) :- string(T1,T2,X).

% Consuming spaces
spaces --> [0' ], spaces. %'
spaces --> [].

% Only lowcased tag names are allowed
tag(N) -->
 letter(H), letters(T),
 { atom_codes(N,[H|T]) }.

% Bare letters
letters([H|T]) --> letter(H), letters(T).
letters([]) --> [].
letter(H,[H|T],T) :- H >= 0'a, H =< 0'z.

