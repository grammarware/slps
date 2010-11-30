/*

We face a non-terminating system by design.
Hence, we must not restrict the result agent to be sort of "empty".
In this manner, we can backtrack through the prefixes of action sequences.

*/

tstar(E,E,[],_).
tstar(E1,E3,[A|As],S) :- t(E1,E2,A,S), tstar(E2,E3,As,S).

