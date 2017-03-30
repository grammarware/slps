:- ['../untyped/term.pro'].
:- ['../untyped/value.pro'].
:- ['../untyped/eval.pro'].
:- ['welltyped.pro'].
:- ['../../shared/main/typed.pro'].

:-
    % Compatibility hack for >6.4.1 and the use of '--'
    ( RawArgv = argv ; RawArgv = os_argv ),
    current_prolog_flag(RawArgv,Argv),
    ( append(_,['--',Input],Argv), main(Input), halt; true ).
