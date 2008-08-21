%%%%%%%%%%%%%%%%%%%%%%%%
% Extract BGF from BTF %
%%%%%%%%%%%%%%%%%%%%%%%%

btf2bgf(T,g([],Ps2))
 :-
    collect(used_p_rule,T,Ps1),
    list_to_set(Ps1,Ps2).

used_p_rule(n(P,_),[P]).
