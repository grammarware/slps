% A root tree is a regular tree with an opening and closing tags and a forest in between
tree(tree(N,AL,TL)) -->
 layout,
 string("<"),
 name(N),
 attrs(AL,_),
 string(">"),
 trees(TL),
 string("</"),
 name(N),
 string(">"),
 layout.

% A non-empty forest
trees([H|T]) -->
 tree(H),
 trees(T).

% An empty forest
trees([]) --> [].

% Attribute list
attrs([H|T],[N|U]) -->
 layout,
 attr(H),
 attrs(T,U),
 {
  H = [N,_],
  not(member(N,U))
 }.
attrs([],[]) --> [].

% Attribute
attr([N,V]) -->
 name(N),
 string("="),
 value(V).

% Bare strings
string([],X,X).
string([H|T1],[H|T2],X) :- string(T1,T2,X).

% Consuming spaces
layout --> [0' ], layout.  % space '
layout --> [0'	], layout. % tab '
layout --> [10], layout.   % newline
layout --> [].

% Only lowcased tag names are allowed
name(N) -->
 letter(H), letters(T),
 { atom_codes(N,[H|T]) }.

% Bare letters
letters([H|T]) --> letter(H), letters(T).
letters([]) --> [].
letter(H,[H|T],T) :- H >= 0'a, H =< 0'z.

value(V) -->
 char(H), chars(T),
 { atom_codes(V,[H|T]) }.

chars([H|T]) --> char(H), chars(T).
chars([]) --> [].
char(H,[H|T],T) :- H >= 0'a, H =< 0'z.
char(H,[H|T],T) :- H >= 0'0, H =< 0'9.
