:- module(gbtf,[generateT/2]).

:- dynamic gbtf:mindepthFact/2.

generateT(G,r(G,T))
 :-

    G = g(Rs,Ps),

% Up-front sanity check
    definedNs(Ps,DNs),
    usedNs(Ps,UNs),
    subtract(UNs,DNs,UDNs),
    require(UDNs == [], 'undefined nonterminals ~w', [UDNs]),
    
% Attempt mindepth computation

    mindepthLoop(Ps),

% Generate shortest completion
    ( Rs = [R|_] ->
        true;
        ( Ps = [p(_,R,_)|_] ) ),
    generateT(Ps,n(R),T).

generateT(_,true,true).

generateT(_,t(V),t(V)).

generateT(Ps,n(N),n(P,T))
 :-
    findN(Ps,N,PsN),
    mindepthFrom(PsN,P),
    P = p(_,N,X),
    generateT(Ps,X,T).

generateT(Ps,s(S,X),s(S,T))
 :-
    generateT(Ps,X,T).

generateT(Ps,','(Xs),','(Ts))
 :-
    maplist(generateT(Ps),Xs,Ts).

generateT(Ps,';'(Xs),';'(X,T))
 :-
    mindepthFrom(Xs,X),
    generateT(Ps,X,T).

generateT(_,'?'(_),'?'([])).

/*
generateT(Ps,'?'(X),'?'([T]))
 :-
    generateT(Ps,X,T).
*/

generateT(_,'*'(_),'*'([])).

/*
generateT(Ps,'*'(X),'*'([T]))
 :-
    generateT(Ps,X,T).
*/

generateT(Ps,'+'(X),'+'([T]))
 :-
    generateT(Ps,X,T).

/*
generateT(Ps,'+'(X),'+'([T1,T2]))
 :-
    generateT(Ps,X,T1),
    generateT(Ps,X,T2).
*/

% ------------------------------------------------------------

% 
% Test mindepth fact base to be complete.
% This is an indirect test for reducedness/termination condition.
% 

mindepthTest(Ps)
 :-
    definedNs(Ps,Ns1),
    findall(N,mindepthFact(N,_),Ns2), 
    subtract(Ns1,Ns2,Ns3),
    require(Ns3 == [],'grammar not reduced/terminated w.r.t. ~w',[Ns3]).


%
% Determine mindepth for all nonterminals
% Loop over productions until fixed point is found
% This is essentially an iterated fixed point computation.
%

mindepthLoop(Ps)
 :-
    mindepthLoop([],Ps).

mindepthLoop(NDs1,Ps)
 :-
    definedNs(Ps,Ns),
    maplisttry(gbtf:mindepthPass(Ps),Ns),
    findall((N,D),mindepthFact(N,D),NDs2),
    ( NDs1 == NDs2 ->
        mindepthTest(Ps);
        mindepthLoop(NDs2,Ps)
    ).

mindepthPass(Ps1,N)
 :-
    findN(Ps1,N,Ps2),
    maplisttry(gbtf:mindepth,Ps2,Ds),
    min1(Ds,D),
    retractall(gbtf:mindepthFact(N,_)),
    assertz(gbtf:mindepthFact(N,D)),
    !.

mindepth(p(_,_,X),D) :- mindepth(X,D).
mindepth(true,0).
mindepth(t(_),0).
mindepth(n(N),D2) :- mindepthFact(N,D1), D2 is D1 + 1.
mindepth(s(_,X),D) :- mindepth(X,D).

mindepth(','(Xs),D)
 :- 
    maplist(gbtf:mindepth,Xs,Ds),
    max1(Ds,D).

mindepth(','(Xs),D)
 :- 
    maplist(gbtf:mindepth,Xs,Ds),
    max1(Ds,D).

mindepth(';'(Xs),D)
 :- 
    maplisttry(gbtf:mindepth,Xs,Ds),
    min1(Ds,D).

mindepth('?'(_),0).
mindepth('*'(_),0).
mindepth('+'(X),D) :-  mindepth(X,D).


%
% Given a list of options, determine the one with mindepth.
% The leftmost is chosen if there are multiple options with mindepth.
%

mindepthFrom([O],O).

mindepthFrom([O1,O2|Os],O)
 :-
    mindepth(O1,D1),
    mindepthFrom(O1,D1,[O2|Os],O).

mindepthFrom(O,_,[],O).

mindepthFrom(O1,D1,[O2|Os],O)
 :-
    mindepth(O2,D2),
    ( D2 < D1 ->
        mindepthFrom(O2,D2,Os,O) ;
        mindepthFrom(O1,D1,Os,O)
    ).
   
% ------------------------------------------------------------
