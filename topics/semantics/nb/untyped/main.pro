:- ['term.pro'].
:- ['value.pro'].
:- ['eval.pro'].
:- ['../../shared/main/untyped.pro'].

:-
    current_prolog_flag(argv,Argv),
    ( append(_,['--',Input],Argv), main(Input), halt; true ).
