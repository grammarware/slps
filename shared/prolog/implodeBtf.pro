%
% Implode BTF trees
%

implodeRoot(r(G,T),(G,V)) :- implodeTree(T,V).

implodeTree(v(string(V)),V).

implodeTree(v(int(V)),V).

implodeTree(t(V),V).

implodeTree(n(p([l(L)],_,_),true),L)
 :-
    !.

implodeTree(n(p([l(L)],_,_),','(Ts1)),T)
 :-
    !,
    maplist(implodeTree,Ts1,Ts2),
    T =.. [L|Ts2].

implodeTree(n(p([l(L)],_,_),T1),T3)
 :-
    !,
    implodeTree(T1,T2),
    T3 =.. [L,T2].

implodeTree(n(p([],_,_),T1),T2)
 :-
    !,
    implodeTree(T1,T2).

implodeTree(s(S,true),S)
 :-
    !.

implodeTree(s(S,','(Ts1)),T)
 :-
    !,
    maplist(implodeTree,Ts1,Ts2),
    T =.. [S|Ts2].

implodeTree(s(S,T1),T3)
 :-
    !,
    implodeTree(T1,T2),
    T3 =.. [S,T2].

implodeTree(';'(_,T1),T2)
 :-
    !,
    implodeTree(T1,T2).

implodeTree(','(Ts1),T)
 :-
    !,
    maplist(implodeTree,Ts1,Ts2),
    list2tuple(Ts2,T).

implodeTree('?'(Ts1),Ts2)
 :-
    !,
    maplist(implodeTree,Ts1,Ts2).

implodeTree('*'(Ts1),Ts2)
 :-
    !,
    maplist(implodeTree,Ts1,Ts2).

implodeTree('+'(Ts1),Ts2)
 :-
    !,
    maplist(implodeTree,Ts1,Ts2).

list2tuple([T],T).
list2tuple([T1|Ts],(T1,T2)) :- list2tuple(Ts,T2).
