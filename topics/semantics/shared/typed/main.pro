% Read, typecheck and evaluate

main(Input)
 :-
    see(Input),
    read(Term),
    seen,
    format('Input term: ~w~n',[Term]),
    welltyped(Term, Type),    
    format('Type of term: ~w~n',[Type]),
    manysteps(Term,Value),
    format('Value of term: ~w~n',[Value]).

