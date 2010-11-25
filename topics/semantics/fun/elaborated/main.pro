:- ['../../shared/main/elaborated.pro'].
:- ['term.pro'].
:- ['value.pro'].
:- ['eval.pro'].
:- ['welltyped.pro'].
:- ['erase.pro'].

:-
    current_prolog_flag(argv,Argv),
    ( append(_,['--',Input],Argv), main(Input), halt; true ).

