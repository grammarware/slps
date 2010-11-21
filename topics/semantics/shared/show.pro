show(zero,0) :- !.
show(succ(X),Z) :- !, show(X,Y), Z is Y + 1.
show(X,X).

