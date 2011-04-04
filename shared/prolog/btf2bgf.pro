%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Map BTF tree to BGF expression %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

btf2x(n(P,_),n(N)) :- P = p(_,N,_).


% Accept BTF shallowly
% That is, do not recurse into production-rooted subtrees.

shallowT(a,a).
shallowT(true,true).
shallowT(t(V),t(V)).
shallowT(n(N),n(p(_,N,_),_)).
shallowT(v(int),v(int(_))).
shallowT(v(string),v(string(_))).
shallowT(s(S,X),s(S,T)) :- shallowT(X,T).
shallowT(*(X),*(Ts)) :- maplist(shallowT(X),Ts).
shallowT(+(X),+(Ts)) :- maplist(shallowT(X),Ts).
shallowT(?(X),?(Ts)) :- maplist(shallowT(X),Ts).
shallowT(','(Xs),','(Ts)) :- maplist(shallowT,Xs,Ts).
shallowT(';'(Xs),';'(X,T)) :- member(X,Xs), shallowT(X,T).


%%%%%%%%%%%%%%%%%%%%%%%%
% Extract BGF from BTF %
%%%%%%%%%%%%%%%%%%%%%%%%

btf2bgf(r(G1,T),G2)
 :-
    !,
    definedNs(G1,Ns),
    btf2bgf(Ns,T,G2).

btf2bgf(Ns,T,g([],Ps3))
 :-
    collect(used_p_rule,T,Ps1),
    list_to_set(Ps1,Ps2),
    filter(unless_v_rules(Ns),Ps2,Ps3).

used_p_rule(n(P,_),[P]).

unless_v_rules(Ns,p(_,N,v(string)))
 :-
    \+ member(N,Ns),
    !,
    fail.

unless_v_rules(_,_).


%
% Check structural integrity of tree
%

checkbtf(T1)
 :-
    T1 = r(G1,T2),
    btf2bgf(T1,G2),
    ( checkbtf(G1,T2) -> 
      true; 
      (
        format('BTF global-level check failed.~n * BGF: ~q~n * BTF: ~q~n',[G1,T2]),
        fail
      )
    ),
    ( subsetG(G2,G1) ->
      true;
      (
        format('BTF subset check failed; invoking diff.~n',[]),
        ppG(G1),
        ppG(G2),
        diffG((
          ('bgf-declared-by-btf',G1),
          ('bgf-used-by-btf',G2))),
        fail
      )
    ),
    !.


%
% checkbtf/2
%

checkbtf(g(_,Ps),r(_,T))
 :-
    checkbtf(Ps,T).

checkbtf(g(_,Ps),T)
 :-
    \+ T = r(_,_),
    checkbtf(Ps,T).

checkbtf(Ps,n(p(_,_,X),T))
 :-
    is_list(Ps),
    checkxt(Ps,X,T).


%
% checkxt/3
%

checkxt(Ps,n(N),n(p(_,N,X),T))
 :-
    checkxt(Ps,X,T),
    !.

checkxt(Ps,n(N),v(string(_)))
 :-
    \+ var(N),
    \+ member(p(_,N,_),Ps),
    !.

checkxt(_,t(V),t(V)) :- !.

checkxt(Ps,s(S,X),s(S,T))
 :-
    \+ var(X),
    checkxt(Ps,X,T),
    !.

checkxt(_,v(string),v(string(_))) :- !.

checkxt(_,v(int),v(int(_))) :- !.

checkxt(_,true,true) :- !.

checkxt(Ps,','(Xs),','(Ts))
 :-
    \+ var(Xs),
    length(Xs,Len),
    length(Ts,Len),
    maplist(checkxt(Ps),Xs,Ts),
    !.

checkxt(Ps,';'(Xs),';'(X,T))
 :-
    \+ var(Xs),
    member(X,Xs),
    checkxt(Ps,X,T),
    !.

checkxt(Ps,'*'(X),'*'(Ts))
 :-
    \+ var(X),
    maplist(checkxt(Ps,X),Ts),
    !.

checkxt(Ps,'+'(X),'+'(Ts))
 :-
    \+ var(X),
    length(Ts,Len),
    Len > 0,    
    maplist(checkxt(Ps,X),Ts),
    !.

checkxt(Ps,'?'(X),'?'(Ts))
 :-
    \+ var(X),
    length(Ts,Len),
    Len =< 1,
    maplist(checkxt(Ps,X),Ts),
    !.

checkxt(_,X,T)
 :-
    format('BTF expression-level check failed.~n * BGF expression: ~q~n * BTF expression: ~q~n',[X,T]),
    fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Print tree as plain string %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For now just do a space every now and then.

ppT(r(_,T)) :- ppT(T), nl.
ppT(n(_,T)) :- ppT(T).
ppT(true).
ppT(t(V)) :- format('~w ',[V]).
ppT(s(_,T)) :- ppT(T).
ppT(','(Ts)) :- maplist(ppT,Ts).
ppT(';'(_,T)) :- ppT(T).
ppT('?'(Ts)) :-  maplist(ppT,Ts).
ppT('*'(Ts)) :-  maplist(ppT,Ts).
ppT('+'(Ts)) :-  maplist(ppT,Ts).

ppT(T)
 :-
     T =.. F/A,
     cease('BTF pretty printer failed for top-level functor ~w/~w',[F,A]),
     halt(1).
