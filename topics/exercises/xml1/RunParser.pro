:- ['Parser.pro'].

main(File)
 :-
    parseFile(File,tree,S).

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

