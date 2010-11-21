% Read and evaluate

main(Input)
 :-
    see(Input),
    read(Term),
    seen,
    format('Input term: ~w~n',[Term]),
    manysteps(Term,Value),
    format('Value of term: ~w~n',[Value]).

