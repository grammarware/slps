% Test framework

test(G)
 :-
    ( G -> P = 'OK'; P = 'FAIL' ),
    format('~w: ~w~n',[P,G]).
