:- module(gbtf,
    [ mindepthG/1
    , mindistG/1
    , completeT/3
    , holeT/5
    , forkG/2
    , contextG/2
    , varyT/3
    ] ).

:- dynamic gbtf:mindepthFact/2.
:- dynamic gbtf:mindistFact/3.


% ------------------------------------------------------------

%
% Determine mindepth for all nonterminals.
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

chooseByMindepth([O],O)
 :-
    mindepthX(O,_).

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
% Determine mindist between nonterminals.
% This is very similar to mindept problem.
%

mindistG(g(_,Ps)) 
 :-
    mindistG(Ps).

mindistG(Ps)
 :-
    is_list(Ps),
    mindist1(Ps),
    mindistStar(Ps).


%
% Initialize mindist relation with direct dependencies
% 

mindist1(Ps)
 :-
      definedNs(Ps,DNs),
      member(DN,DNs),
      findN(Ps,DN,PsDN),
      usedNs(PsDN,UNs),
      member(UN,UNs),
      \+ UN == DN,
      assertz(gbtf:mindistFact(DN,UN,1)),
      fail;
      true.


%
% Compute mindists between so-far unrelated nonterminals.
% Use a helper nonterminal in between and existing relationships with it.
% Fixed point is reached if no new relationships are obtainable in this manner.
%

mindistStar(Ps)
 :-
    mindist2(Ps,NNDs),
    ( NNDs == [] ->
        true;
        (
          member((N1,N2,D),NNDs),
          assertz(gbtf:mindistFact(N1,N2,D)),
          fail
        ;
          mindistStar(Ps)
        )
    ). 

mindist2(Ps,NNDs)
 :-
    definedNs(Ps,DNs),
    findall((N1,N2,D),
      (
        member(N1,DNs),
        member(N2,DNs),
        \+ N1 == N2,
        \+ gbtf:mindistFact(N1,N2,_),
        findall(D12,
        (
          gbtf:mindistFact(N1,N3,D1),
          gbtf:mindistFact(N3,N2,D2),
          D12 is D1 + D2
        ),
        Ds),
        min1(Ds,D)
      ),
      NNDs).     


%
% Determine mindist from a BGF expression to a nonterminal
%

mindistX(p(_,_,X),N,D)
 :-
    !,
    mindistX(X,N,D).

mindistX(X,N,D)
 :-
    usedNs(X,UNs),
    findall(D1,
      (
        member(UN,UNs),
        ( UN == N -> D1 = 0; gbtf:mindistFact(UN,N,D1) )
      ),
      Ds),
    min1(Ds,D).


%
% Given a list of options, determine the one with mindist.
% The leftmost is chosen if there are multiple options with mindist.
%

chooseByMindist([O1|Os],N,O)
 :-
    mindistX(O1,N,D1) ->
      chooseByMindist(O1,D1,Os,N,O) ;
      chooseByMindist(Os,N,O).

chooseByMindist(O,_,[],_,O).

chooseByMindist(O1,D1,[O2|Os],N,O)
 :-
    mindistX(O2,N,D2) ->
      ( D2 < D1 ->
          chooseByMindist(O2,D2,Os,N,O) ;
          chooseByMindist(O1,D1,Os,N,O)
      ) ;
      chooseByMindist(O1,D1,Os,N,O).


% ------------------------------------------------------------

%
% Generate shortest completion
%

completeT(G,P,n(P,T))
 :-
    P = p(_,_,X),
    completeT(G,X,T).

completeT(G,n(N),T) :- 
    findN(G,N,Ps),
    chooseByMindepth(Ps,P),
    completeT(G,P,T).

completeT(_,true,true).
completeT(_,t(V),t(V)).
completeT(G,s(S,X),s(S,T)) :- completeT(G,X,T).
completeT(G,','(Xs),','(Ts)) :- maplist(gbtf:completeT(G),Xs,Ts).
completeT(G,';'(Xs),';'(X,T)) :- chooseByMindepth(Xs,X), completeT(G,X,T).
completeT(_,'?'(_),'?'([])).
completeT(_,'*'(_),'*'([])).
completeT(G,'+'(X),'+'([T])) :- completeT(G,X,T).


% ------------------------------------------------------------

%
% Generate shortest completion with hole.
% Parameters:
%  - Grammar
%  - Phrase for completion
%  - Nonterminal for hole
%  - Tree with hole
%  - Logic variable for the hole.
%

holeT(_,n(N),H,V,V) :- N == H.

holeT(G,n(N),H,n(P,T),V)
 :-
    \+ N == H,
    findN(G,N,Ps),
    chooseByMindist(Ps,H,P),
    P = p(_,N,X),
    holeT(G,X,H,T,V).

holeT(G,s(S,X),H,s(S,T),V) :- holeT(G,X,H,T,V).
holeT(G,'?'(X),H,'?'([T]),V) :- holeT(G,X,H,T,V).
holeT(G,'*'(X),H,'*'([T]),V) :- holeT(G,X,H,T,V).
holeT(G,'+'(X),H,'+'([T]),V) :- holeT(G,X,H,T,V).

holeT(G,','(Xs),H,','(Ts),V)
 :-
    chooseByMindist(Xs,H,X),
    once(append(Xs1,[X|Xs2],Xs)),
    holeT(G,X,H,T,V),
    maplist(gbtf:completeT(G),Xs1,Ts1),
    maplist(gbtf:completeT(G),Xs2,Ts2),
    append(Ts1,[T|Ts2],Ts).

holeT(G,';'(Xs),H,';'(X,T),V)
 :-
    chooseByMindist(Xs,H,X),
    holeT(G,X,H,T,V).


% ------------------------------------------------------------

%
% Mark all forks for BC (other than entire productions themselves).
% Nonterminal references are not forks, but they are contexts; see below.
%

forkG(p(L,N,X1),p(L,N,X2))
 :-
    forkG(X1,X2).

forkG(';'(Xs),{';'(Xs)}).
forkG('?'(X),{'?'(X)}).
forkG('*'(X),{'*'(X)}).
forkG('+'(X),{'+'(X)}).

forkG(s(S,X1),s(S,X2)) :- forkG(X1,X2).
forkG('?'(X1),'?'(X2)) :- forkG(X1,X2).
forkG('*'(X1),'*'(X2)) :- forkG(X1,X2).
forkG('+'(X1),'+'(X2)) :- forkG(X1,X2).

forkG(','(Xs1),','(Xs2))
 :-
    append(Xs1a,[X1|Xs1b],Xs1),
    forkG(X1,X2),
    append(Xs1a,[X2|Xs1b],Xs2).

forkG(';'(Xs1),';'(Xs2))
 :-
    append(Xs1a,[X1|Xs1b],Xs1),
    forkG(X1,X2),
    append(Xs1a,[X2|Xs1b],Xs2).


% ------------------------------------------------------------

%
% Find all contexts for CDBC; mark them.
% Return just the marked production (as opposed to the entire grammar).
% Backtracking over this predicate gives all such marked contexts.
%

contextG(p(L,N,X1),p(L,N,X2))
 :-
    contextG(X1,X2).

contextG(n(N),{n(N)}).
contextG(';'(Xs),{';'(Xs)}).
contextG('?'(X),{'?'(X)}).
contextG('*'(X),{'*'(X)}).
contextG('+'(X),{'+'(X)}).

contextG(s(S,X1),s(S,X2)) :- contextG(X1,X2).
contextG('?'(X1),'?'(X2)) :- contextG(X1,X2).
contextG('*'(X1),'*'(X2)) :- contextG(X1,X2).
contextG('+'(X1),'+'(X2)) :- contextG(X1,X2).

contextG(','(Xs1),','(Xs2))
 :-
    append(Xs1a,[X1|Xs1b],Xs1),
    contextG(X1,X2),
    append(Xs1a,[X2|Xs1b],Xs2).

contextG(';'(Xs1),';'(Xs2))
 :-
    append(Xs1a,[X1|Xs1b],Xs1),
    contextG(X1,X2),
    append(Xs1a,[X2|Xs1b],Xs2).


% ------------------------------------------------------------

%
% Exercise choices for the marked BGF expression.
% There must be a marker for this predicate to succeed at all.
% All unmarked particles are completed in the shortest manner.
%

varyT(G,P1,n(P2,T))
 :-
    P1 = p(_,_,X),
    varyT(G,X,T),
    unmarkG(P1,P2).

%
% Treat horizontal definitions as vertical ones.
% Inline chain productions on the fly.
%

varyT(G,{n(N)},n(P,T))
 :-
    findN(G,N,Ps),
    ( (Ps = [P], P = p(_,_,';'(Xs))) ->
        varyT(G,{';'(Xs)},T) 
      ; ( (Ps = [P], P = p(_,_,n(M))) ->
            varyT(G,{n(M)},T)
          ; (
              member(P,Ps),
              P = p(_,_,X),
              completeT(G,X,T)
            )
        )
    ).

varyT(G,{';'(Xs)},';'(X,T))
 :-
    member(X,Xs),
    completeT(G,X,T).

varyT(_,{'?'(_)},'?'([])).
varyT(G,{'?'(X)},'?'([T])) :- completeT(G,X,T).
varyT(_,{'*'(_)},'*'([])).
varyT(G,{'*'(X)},'*'([T])) :- completeT(G,X,T).
varyT(G,{'+'(X)},'+'([T])) :- completeT(G,X,T).
varyT(G,{'+'(X)},'+'([T1,T2])) :- completeT(G,X,T1), completeT(G,X,T2).

varyT(G,s(S,X),s(S,T)) :- varyT(G,X,T).
varyT(G,'?'(X),'?'([T])) :- varyT(G,X,T).
varyT(G,'*'(X),'*'([T])) :- varyT(G,X,T).
varyT(G,'+'(X),'+'([T])) :- varyT(G,X,T).

varyT(G,','(Xs),','(Ts))
 :-
    append(Xs1,[X|Xs2],Xs),
    maplist(gbtf:completeT(G),Xs1,Ts1),
    varyT(G,X,T),
    maplist(gbtf:completeT(G),Xs2,Ts2),
    append(Ts1,[T|Ts2],Ts). 

varyT(G,';'(Xs),';'(X2,T))
 :-
    member(X1,Xs),
    varyT(G,X1,T), 
    unmarkG(X1,X2).


% ------------------------------------------------------------
