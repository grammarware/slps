:- module(xbgf2,[transformT/3]).


transformT(T,T1,T2)
 :-
    T =.. [F|_],
    format('Applying ~q transformation.~n',[F]),
    apply(T,[T1,T2]),
    !.


%
% p([l(add)], f, n(p))
%
% Add a production to an existing definition
%


%
% p([l(define)], f, +n(p))
%
% Define a nonterminal
%

define(_,T1,T1)
 :-
    !.


%
% p([l(downcase)], f, true)
%
% Establish lowercase for all sorts of symbols
%

downcase(T1,T1) 
 :-
    !.


%
% p([l(eliminate)], f, n(n))
%
% Eliminate a defined, otherwise unused nonterminal
%


%
% p([l(extract)], f, n(p))
%
% Extract a nonterminal definition
%

extract(_,T1,T1)
 :-
    !.


%
% p([l(fold)], f, n(p))
%
% Fold an expression to its defining nonterminal
%

fold(_,T1,T1)
 :-
    !.


%
% p([l(horizontal)], f, n(n))
%
% Turn multiple productions into choice
%


%
% p([l(id)], f, true)
%
% Identity
%

id(T1,T1).


%
% p([l(inline)], f, n(n))
%
% Inline a nonterminal definition (and eliminate it)
%

inline(_,T1,T1)
 :- 
    !.
    

%
% p([l(introduce)], f, +n(p))
%
% Add a definition for a fresh nonterminal
%

introduce(_,T1,T1)
 :-
    !.


%
% p([l(label)], f, n(p))
%
% Label a production
%

label(_,T1,T1)
 :-
    !.

%
% p([l(massage)], f, n(p))
%
% Obviously correct rewrites
%

massage(_,T1,T1)
 :-
    !.


%
% p([l(permute)], f, n(p))
%
% Permute the body of a production
%

permute(_,T1,T1)
 :- 
    !.


%
% p([l(project)], f, n(p))
%
% Apply projection to the body of a production
%

%
% p([l(prune)], f, n(n))
%
% Prune nonterminals
% (Assume epsilon as missing definition)
%

prune(_,T1,T1)
 :-
    !.
    

% p([l(relax)], f, n(p))

% p([l(relabel)], f, n(p))


%
% p([l(remove)], f, n(p))
%
% Remove a production
%

remove(_,T1,T1)
 :- 
    !.   


%
% p([l(renameL)], f, ','([n(l), n(l)]))
% p([l(renameN)], f, ','([n(n), n(n)]))
% p([l(renameS)], f, ','([?(n(l)), n(s), n(s)]))
% p([l(renameT)], f, ','([n(t), n(t)]))
%
% Rename labels, nonterminals, selectors, and terminals
%

renameL((L1,L2),T1,T2)
 :-
    renameL(L1,L2,T1,T2).

renameL(_,_,T1,T1)
 :-
    !.

renameN((N1,N2),T1,T2)
 :-
    renameN(N1,N2,T1,T2).

renameN(_,_,T1,T1)
 :-
    !.

renameS((S1,S2),T1,T2)
 :-
    renameS([],S1,S2,T1,T2).

renameS([],_,_,T1,T1)
 :-
    !.

renameS([_],_,_,T1,T1)
 :-
    !.


%
% p([l(reroot)], f, *(n(n)))
%
% Assign new roots to the grammar
%

reroot(_,T1,T1)
 :-
    !.


%
% p([l(restrict)], f, n(p))
%
% Restrict the grammar by expression replacement
%

restrict(_,T1,T1)
 :-
    !.


% p([l(sequence)], f, *(n(f)))

sequence(Ts,T1,T2)
 :-
    accum(xbgf2:transformT,Ts,T1,T2).


%
% p([l(skip)], f, n(p))
%
% Skip a production
%

skip(_,T1,T1)
 :- 
    !.


%
% p([l(stripL)], f, n(l))
% p([l(stripLs)], f, true)
% p([l(stripS)], f, n(s))
% p([l(stripSs)], f, true)
% p([l(stripT)], f, n(t))
% p([l(stripTs)], f, true)
%
% Strip labels, selectors, and terminals
%

stripL(_,T1,T1)
 :-
    !.

stripLs(T1,T1)
 :-
    !.

stripS(_,T1,T1)
 :-
    !.

stripSs(T1,T1)
 :-
    !.

stripTs(T1,T1)
 :-
    !.

stripT(_,T1,T1) 
 :-
    !.


%
% p([l(unchain)], f, n(n))
%
% Unchain a nonterminal -- a restricted unfold
%

unchain(_,T1,T1)
 :-
    !.


%
% p([l(undefine)], f, n(n))
%
% Undefine a nonterminal, i.e., remove all productions
%

undefine(_,T1,T1)
 :-
    !.


%
% p([l(unfold)], f, n(p))
%
% Unfold a nonterminal in a production
%

unfold(_,T1,T1)
 :-
    !.


%
% p([l(unite)], f, ','([n(n), n(n)]))
%
% Confusing renaming, also called "unification"
%

unite(_,_,T1,T1)
 :-
    !.


%
% p([l(verticalL)], f, n(l))
% p([l(verticalN)], f, n(n))
%
% Turn choices into definitions of multiple productions
%

verticalL(_,T1,T1)
 :-
    !.

verticalN(_,T1,T1)
 :-
    !.
