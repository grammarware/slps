:- ['term.pro'].
:- ['value.pro'].
:- ['eval.pro'].

% Read and evaluate

main(Input)
 :-
    see(Input),
    read(Term),
    seen,
    format('Input term: ~w~n',[Term]),
    manysteps(Term,Value),
    format('Value of term: ~w~n',[Value]).

:-
    current_prolog_flag(argv,Argv),
    ( append(_,['--',Input],Argv), main(Input), halt; true ).
