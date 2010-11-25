% Read, typecheck, elaborate and evaluate

:- ensure_loaded('../elaborate.pro').
:- ensure_loaded('../show.pro').

main(Input)
 :-
    see(Input),
    read(Term),
    seen,
    format('Input term: ~w~n',[Term]),
    welltyped(Term, Type),    
    format('Type of term: ~w~n',[Type]),
    elaborate(Term,E),
    format('Elaborated term: ~w~n',[E]),
    manysteps(E,X),
    show(X,Y),
    format('Value of term: ~w~n',[Y]).

