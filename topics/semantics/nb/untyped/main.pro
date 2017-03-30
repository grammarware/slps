:- ['term.pro'].
:- ['value.pro'].
:- ['eval.pro'].
:- ['../../shared/main/untyped.pro'].

:-
    % Compatibility hack for >6.4.1 and the use of '--'
    ( RawArgv = argv ; RawArgv = os_argv ),
    current_prolog_flag(RawArgv,Argv),
    ( append(_,['--',Input],Argv), main(Input), halt; true ).
