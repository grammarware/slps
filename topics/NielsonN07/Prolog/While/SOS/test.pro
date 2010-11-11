test(G,L1)
 :-
    findall(R,apply(G,[R]),L2),
    ( L1 = L2 -> P = 'OK'; P = 'FAIL'),
    format('~w: ~w -> ~w~n',[P,G,L2]).
