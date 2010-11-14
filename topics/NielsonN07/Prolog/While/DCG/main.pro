:- ['parser.pro'].
:- ['parselib.pro'].

main(File) :-
 parseFile(File,program,S),
 write(S),nl.

