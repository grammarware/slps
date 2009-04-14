file(File) :- see(File), read(X), seen, write(X), nl.

files([]).
files([File|Files]) :- write(','), file(File), files(Files).

:-
   unix(argv(Argv)), append(_,[--,File|Files],Argv),
   write('([],['), nl,
   file(File),
   files(Files),
   write(']).'), nl,
   halt.

