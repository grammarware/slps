:- ['eval.pro'].
:- ['ns.pro'].
:- ['map.pro'].

% Tests

test(G)
 :-
    ( G -> P = 'OK'; P = 'FAIL' ),
    format('~w: ~w~n',[P,G]).

:- test(evala(add(num(21),num(21)),_,42)).
:- test(evala(add(num(21),id(x)),[('x',21)],42)).
:- test(
    execute(
     while( not(eq(id(x),num(0))),
            seq(
              assign(y,mul(id(x),id(y))),
              assign(x,sub(id(x),num(1))))),
     [(x,5),(y,1)],
     [(x,0),(y,120)])).

:- halt.
