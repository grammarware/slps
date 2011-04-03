:- module(gbtf,
    [ mindepthG/1
    , contextG/2
    , shortestG/2
    ] ).

:- dynamic gbtf:mindepthFact/2.


% ------------------------------------------------------------

%
% Determine mindepth for all nonterminals
% Loop over productions until fixed point is found
% This is essentially an iterated fixed point computation.
%

mindepthG(g(_,Ps))
 :-
    mindepthG(Ps).

mindepthG(Ps)
 :-
    is_list(Ps),

% Up-front sanity check
    definedNs(Ps,DNs),
    usedNs(Ps,UNs),
    subtract(UNs,DNs,UDNs),
    require(UDNs == [], 'undefined nonterminals ~w', [UDNs]),

% Enter loop
    mindepthG([],Ps).

mindepthG(NDs1,Ps)
 :-
    definedNs(Ps,Ns),
    maplisttry(gbtf:mindepthN(Ps),Ns),
    findall((N,D),mindepthFact(N,D),NDs2),
    ( NDs1 == NDs2 ->
        testMindepth(Ps);
        mindepthG(NDs2,Ps)
    ).


% 
% Test mindepth fact base to be complete.
% This is an indirect test for reducedness/termination condition.
% 

testMindepth(Ps)
 :-
    definedNs(Ps,Ns1),
    findall(N,mindepthFact(N,_),Ns2), 
    subtract(Ns1,Ns2,Ns3),
    require(Ns3 == [],'grammar not reduced/terminated w.r.t. ~w',[Ns3]).


%
% Determine mindepth for the productions of a nonterminal
% Existing mindepth facts are leveraged.
%

mindepthN(Ps1,N)
 :-
    findN(Ps1,N,Ps2),
    maplisttry(gbtf:mindepthX,Ps2,Ds),
    min1(Ds,D),
    retractall(gbtf:mindepthFact(N,_)),
    assertz(gbtf:mindepthFact(N,D)),
    !.


%
% Compute mindepth for a given BGF expression
%

mindepthX(p(_,_,X),D) :- mindepthX(X,D).
mindepthX(true,0).
mindepthX(t(_),0).
mindepthX(n(N),D2) :- mindepthFact(N,D1), D2 is D1 + 1.
mindepthX(s(_,X),D) :- mindepthX(X,D).
mindepthX('?'(_),0).
mindepthX('*'(_),0).
mindepthX('+'(X),D) :-  mindepthX(X,D).

mindepthX(','(Xs),D)
 :- 
    maplist(gbtf:mindepthX,Xs,Ds),
    max1(Ds,D).

mindepthX(';'(Xs),D)
 :- 
    maplisttry(gbtf:mindepthX,Xs,Ds),
    min1(Ds,D).


%
% Given a list of options, determine the one with mindepth.
% The leftmost is chosen if there are multiple options with mindepth.
%

chooseByMindepth([O],O).

chooseByMindepth([O1,O2|Os],O)
 :-
    mindepthX(O1,D1),
    chooseByMindepth(O1,D1,[O2|Os],O).

chooseByMindepth(O,_,[],O).

chooseByMindepth(O1,D1,[O2|Os],O)
 :-
    mindepthX(O2,D2),
    ( D2 < D1 ->
        chooseByMindepth(O2,D2,Os,O) ;
        chooseByMindepth(O1,D1,Os,O)
    ).

   
% ------------------------------------------------------------

%
% Find all contexts for CDBC; mark them.
% Backtracking over this predicate gives all such marked contexts.
%

contextG(g(Rs,Ps1),g(Rs,Ps2))
 :-
    contextG(Ps1,Ps2).

contextG(Ps1,Ps2)
 :-
    is_list(Ps1),
    append(Ps1a,[P1|Ps1b],Ps1),
    P1 = p(L,N,X1),
    contextX(X1,X2),
    P2 = p(L,N,X2),
    append(Ps1a,[P2|Ps1b],Ps2).


%
% These are contexts.
%

contextX(n(N),{n(N)}).
contextX('?'(X),{'?'(X)}).
contextX('*'(X),{'*'(X)}).
contextX('+'(X),{'+'(X)}).
contextX(';'(Xs),{';'(Xs)}).


%
% These are compounds with context potential.
%

contextX(s(S,X1),s(S,X2)) :- contextX(X1,X2).
contextX('?'(X1),'?'(X2)) :- contextX(X1,X2).
contextX('*'(X1),'*'(X2)) :- contextX(X1,X2).
contextX('+'(X1),'+'(X2)) :- contextX(X1,X2).

contextX(','(Xs1),','(Xs2))
 :-
    append(Xs1a,[X1|Xs1b],Xs1),
    contextX(X1,X2),
    append(Xs1a,[X2|Xs1b],Xs2).

contextX(';'(Xs1),';'(Xs2))
 :-
    append(Xs1a,[X1|Xs1b],Xs1),
    contextX(X1,X2),
    append(Xs1a,[X2|Xs1b],Xs2).


% ------------------------------------------------------------

%
% Generate shortest completion
%

shortestG(G,r(G,T))
 :-
    G = g(Rs,Ps),
    ( Rs = [R|_] ->
        true;
        ( Ps = [p(_,R,_)|_] ) ),
    shortestX(Ps,n(R),T).

shortestX(_,true,true).
shortestX(_,t(V),t(V)).
shortestX(Ps,s(S,X),s(S,T)) :- shortestX(Ps,X,T).
shortestX(Ps,','(Xs),','(Ts)) :- maplist(gbtf:shortestX(Ps),Xs,Ts).
shortestX(Ps,';'(Xs),';'(X,T)) :- chooseByMindepth(Xs,X), shortestX(Ps,X,T).
shortestX(_,'?'(_),'?'([])).
shortestX(_,'*'(_),'*'([])).
shortestX(Ps,'+'(X),'+'([T])) :- shortestX(Ps,X,T).

shortestX(Ps,n(N),n(P,T))
 :-
    findN(Ps,N,PsN),
    chooseByMindepth(PsN,P),
    P = p(_,N,X),
    shortestX(Ps,X,T).

% ------------------------------------------------------------
