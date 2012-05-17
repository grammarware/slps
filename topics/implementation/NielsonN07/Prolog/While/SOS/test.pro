/*

test/2 tests non-deterministic predicates with a final result position.

Consider this predicate.

foo(a,1).
foo(b,2).
foo(a,3).

% Now consider this test case:

:- test(foo(a),[1,3]).

*/

test(G,L1)
 :-
    findall(R,apply(G,[R]),L2),
    ( L1 = L2 -> P = 'OK'; P = 'FAIL'),
    format('~w: ~w -> ~w~n',[P,G,L2]).
