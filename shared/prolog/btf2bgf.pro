%%%%%%%%%%%%%%%%%%%%%%%%
% Extract BGF from BTF %
%%%%%%%%%%%%%%%%%%%%%%%%

btf2bgf(T,g([],Ps2))
 :-
    collect(used_p_rule,T,Ps1),
    list_to_set(Ps1,Ps2).

used_p_rule(n(P,_),[P]).


%
% Check structural integrity of tree
%

checkbtf(n(p(_,_,X),T))
 :-
    checkbtf(X,T).

checkbtf(n(N),n(p(_,N,X),T))
 :-
    !,
    checkbtf(X,T).

checkbtf(t(V),t(V))
 :-
    !.

checkbtf(v(string),v(string(_)))
 :-
    !.

checkbtf(v(int),v(int(_)))
 :-
    !.

checkbtf(true,true)
 :-
    !.

checkbtf(','(Xs),','(Ts))
 :-
    !,
    maplist(checkbtf,Xs,Ts).

checkbtf(';'(Xs),';'(X,T))
 :-
    member(X,Xs),
    !,
    checkbtf(X,T).

checkbtf('*'(X),'*'(Ts))
 :-
    !,
    maplist(checkbtf(X),Ts).

checkbtf('+'(X),'+'(Ts))
 :-
    length(Ts,Len),
    Len > 0,    
    !,
    maplist(checkbtf(X),Ts).

checkbtf('?'(X),'?'(Ts))
 :-
    length(Ts,Len),
    Len =< 1,    
    !,
    maplist(checkbtf(X),Ts).

checkbtf(X,T)
 :-
    X =.. [F1|L1],
    T =.. [F2|L2],
    length(L1,N1),
    length(L2,N2),
    format('BTF check failed: grammar level: ~q/~q; tree level: ~q/~q.~n',[F1,N1,F2,N2]),
    fail.
