:- ['Parser.pro','mappings.pro','compExpr.pro','smallStm.pro','Types.pro'].

main(File) :-
 parseFile(File,program,S),
 write(S),nl,nl,
 wtype(S,[],M,_),
 %execute(S,[],M),
 write(M),nl.

parseFile(File,P,R) :-
 open(File,read,Stream,[]), 
 read_stream_to_codes(Stream, Contents),
 close(Stream),
 apply(P,[R,Contents,Rest]),
 eof(Rest,_).

eof([],[]).
eof([0' |T],R) :- eof(T,R). %'
eof([10|T],R) :- eof(T,R).

