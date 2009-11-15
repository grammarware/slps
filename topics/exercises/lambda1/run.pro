:- ['parser.pro','syntax.pro'].

main(File) :-
 parseFile(File,expression,E),
 term(E),
 write(E),nl.

parseFile(File,P,R) :-
 open(File,read,Stream,[]), 
 read_stream_to_codes(Stream, Contents),
 close(Stream),
 apply(P,[R,Contents,Rest]),
 eof(Rest,_).

eof([],[]).
eof([0' |T],R) :- eof(T,R). %'
eof([10|T],R) :- eof(T,R).
