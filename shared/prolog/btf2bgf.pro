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

checkbtf(Ps,n(p(_,_,X),T))
 :-
    checkbtf(Ps,X,T).

checkbtf(Ps,n(N),n(p(_,N,X),T))
 :-
    !,
    checkbtf(Ps,X,T).

checkbtf(Ps,n(N),v(string(_)))
 :-
    \+ member(p(_,N,_),Ps),
    !.

checkbtf(_,t(V),t(V))
 :-
    !.

checkbtf(_,v(string),v(string(_)))
 :-
    !.

checkbtf(_,v(int),v(int(_)))
 :-
    !.

checkbtf(_,true,true)
 :-
    !.

checkbtf(Ps,','(Xs),','(Ts))
 :-
    !,
    maplist(checkbtf(Ps),Xs,Ts).

checkbtf(Ps,';'(Xs),';'(X,T))
 :-
    member(X,Xs),
    !,
    checkbtf(Ps,X,T).

checkbtf(Ps,'*'(X),'*'(Ts))
 :-
    !,
    maplist(checkbtf(Ps,X),Ts).

checkbtf(Ps,'+'(X),'+'(Ts))
 :-
    length(Ts,Len),
    Len > 0,    
    !,
    maplist(checkbtf(Ps,X),Ts).

checkbtf(Ps,'?'(X),'?'(Ts))
 :-
    length(Ts,Len),
    Len =< 1,    
    !,
    maplist(checkbtf(Ps,X),Ts).

checkbtf(_,X,T)
 :-
    X =.. [F1|L1],
    T =.. [F2|L2],
    length(L1,N1),
    length(L2,N2),
    format('BTF check failed: grammar level: ~q/~q; tree level: ~q/~q.~n',[F1,N1,F2,N2]),
    fail.
