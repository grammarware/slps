tree(tree(N,L)) -->
 spaces,
 string("<"),
 tagname(N),
 string(">"),
 trees(L),
 string("</"),
 tagname(N),
 string(">"),
 spaces.

trees([H|T]) -->
 tree(H),
 trees(T).
trees([]) --> [].

string([],X,X).
string([H|T1],[H|T2],X) :- string(T1,T2,X).

spaces --> [0' ], spaces. %'
spaces --> [].

tagname(N) -->
 letter(H), letters(T),
 { atom_codes(N,[H|T]) }.

letters([H|T]) --> letter(H), letters(T).
letters([]) --> [].
letter(H,[H|T],T) :- H >= 0'a, H =< 0'z.
