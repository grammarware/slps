% Read and evaluate

main(Input)
 :-
    see(Input),
    read(Term),
    seen,
    format('Input term: ~w~n',[Term]),
    manysteps(Term,X),
    show(X,Y),
    format('Value of term: ~w~n',[Y]).

:- ensure_loaded('../show.pro').

