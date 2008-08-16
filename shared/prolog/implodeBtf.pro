%
% Implode BTF trees
%

implodeT(v(string(V)),V).

implodeT(v(int(V)),V).

implodeT(t(V),V).

implodeT(n(p([l(L)],_,_),true),L)
 :-
    !.

implodeT(n(p([l(L)],_,_),','(Ts1)),T)
 :-
    !,
    maplist(implodeT,Ts1,Ts2),
    T =.. [L|Ts2].

implodeT(n(p([l(L)],_,_),T1),T3)
 :-
    !,
    implodeT(T1,T2),
    T3 =.. [L,T2].

implodeT(n(p([],_,_),T1),T2)
 :-
    !,
    implodeT(T1,T2).

implodeT(s(S,true),S)
 :-
    !.

implodeT(s(S,','(Ts1)),T)
 :-
    !,
    maplist(implodeT,Ts1,Ts2),
    T =.. [S|Ts2].

implodeT(s(S,T1),T3)
 :-
    !,
    implodeT(T1,T2),
    T3 =.. [S,T2].

implodeT(';'(_,T1),T2)
 :-
    !,
    implodeT(T1,T2).

implodeT(','(Ts1),T)
 :-
    !,
    maplist(implodeT,Ts1,Ts2),
    list2tuple(Ts2,T).

implodeT('?'(Ts1),Ts2)
 :-
    !,
    maplist(implodeT,Ts1,Ts2).

implodeT('*'(Ts1),Ts2)
 :-
    !,
    maplist(implodeT,Ts1,Ts2).

implodeT('+'(Ts1),Ts2)
 :-
    !,
    maplist(implodeT,Ts1,Ts2).

list2tuple([T],T).
list2tuple([T1|Ts],(T1,T2)) :- list2tuple(Ts,T2).
