:- ['../untyped/term.pro'].
:- ['../untyped/value.pro'].
:- ['../untyped/eval.pro'].
:- ['welltyped.pro'].
:- ['../../shared/main/typed.pro'].

:-
    current_prolog_flag(argv,Argv),
    ( append(_,['--',Input],Argv), main(Input), halt; true ).
