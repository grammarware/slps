% EBNF

many1(P,X,Z) :- apply(P,[X,Y]), many(P,Y,Z).
many(P,X,Y) :- many1(P,X,Y).
many(_,X,X).

many1(P,[H|T],X,Z) :- apply(P,[H,X,Y]), many(P,T,Y,Z).
many(P,L,X,Y) :- many1(P,L,X,Y).
many(_,[],X,X).

option(_,P,R,X,Y) :- apply(P,[R,X,Y]).
option(D,_,D,X,X).


% Expressions combined with left-associative infix operators

lassoc(O,P,F,R,A,C) :-
  apply(P,[X,A,B]),
  lassoc_rest(O,P,F,X,R,B,C).

lassoc_rest(O,P,F,X,Z,A,D) :-
  apply(O,[OR,A,B]),
  apply(P,[Y,B,C]),
  T =.. [F,OR,X,Y],
  lassoc_rest(O,P,F,T,Z,C,D).

lassoc_rest(_,_,_,X,X,A,A).


% Parse from a file

parseFile(File,P,R)
 :-
    open(File,read,Stream,[]), 
    read_stream_to_codes(Stream, Contents),
    close(Stream),
    apply(P,[R,Contents,Rest]),
    eof(Rest,_).

eof([],[]).
eof([0' |T],R) :- eof(T,R). %'
eof([10|T],R) :- eof(T,R).


% Load-ahead test

follows(P,[H|T],[H|T]) :- apply(P,[H]).


% Predicates for character sets

isDigit(X) :- X >= 0'0, X =< 0'9.
isLower(X) :- X >= 0'a, X =< 0'z.
isSpace(0' ). % '
isSpace(X) :- isEoln(X).
isEoln(10).


% Scanning characters and sequences thereof based on a predicate 

satisfy(P,H,[H|T],T) :- apply(P,[H]).
munch(P,[H|T]) --> satisfy(P,H), !, munch(P,T). % no lexical backtracking
munch(_,[]) --> [].
munch1(P,[H|T]) --> satisfy(P,H), munch(P,T).


% Scanning characters and contiguous sequences thereof

char(H,H,[H|T],T).

string([],R,R).
string([H|T1],[H|T2],R) :- !, string(T1,T2,R). % no lexical backtracking
