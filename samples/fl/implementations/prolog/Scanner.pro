% Predicates for character sets

isDigit(X) :- X >= 0'0, X =< 0'9.
isLower(X) :- X >= 0'a, X =< 0'z.
isSpace(0' ). % '
isEoln(10).
isWhitespace(X) :- isSpace(X).
isWhitespace(X) :- isEoln(X).


% Scanning characters and sequences thereof based on a predicate 

satisfy(P,H,[H|T],T) :- apply(P,[H]).
munch(P,[H|T]) --> satisfy(P,H), !, munch(P,T). % no lexical backtracking
munch(_,[]) --> [].
munch1(P,[H|T]) --> satisfy(P,H), munch(P,T).


% Scanning characters and contiguous sequences thereof

char(H,H,[H|T],T).

string([],R,R).
string([H|T1],[H|T2],R) :- !, string(T1,T2,R). % no lexical backtracking


% Token classes with withspace handling

spaces --> munch(isSpace,_).

int(I) --> 
    spaces,
    option(0'+,char(0'-),Sign),
    munch1(isDigit,Digits),
    { name(I,[Sign|Digits]) } .

name(N) --> spaces, munch1(isLower,S), { name(N,S) }.

special(S) --> spaces, string(S).

eoln --> spaces, satisfy(isEoln,_).

eof([],[]).

keyword(S) --> spaces, string(S), ( follows(isWhitespace) ; eof ).


% Test look-ahead character

follows(P,[H|T],[H|T]) :- apply(P,[H]).
