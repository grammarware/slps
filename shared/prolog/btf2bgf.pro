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

checkbtf(_,T)
 :-
    T =.. [F|L],
    length(L,N),
    format('BTF checker: cannot handle ~w/~w.~n',[F,N]),
    fail.
