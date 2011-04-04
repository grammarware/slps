:- module(gbtf,
    [ mindepthG/1
    , mindistG/1
    , completeG/3
    , hostG/5
    , contextN/3
    , varyG/3
    , varyN/3
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

completeG(g(_,Ps),N,T)
 :-
    completeG(Ps,N,T).

completeG(Ps,N,n(P,T))
 :-
    is_list(Ps),
    findN(Ps,N,PsN),
    chooseByMindepth(PsN,P),
    completeP(Ps,P,T).

completeP(Ps,P,T)
 :-
    P = p(_,_,X),
    completeX(Ps,X,T).

completeX(_,true,true).
completeX(_,t(V),t(V)).
completeX(Ps,n(N),T) :- completeG(Ps,N,T).
completeX(Ps,s(S,X),s(S,T)) :- completeX(Ps,X,T).
completeX(Ps,','(Xs),','(Ts)) :- maplist(gbtf:completeX(Ps),Xs,Ts).
completeX(Ps,';'(Xs),';'(X,T)) :- chooseByMindepth(Xs,X), completeX(Ps,X,T).
completeX(_,'?'(_),'?'([])).
completeX(_,'*'(_),'*'([])).
completeX(Ps,'+'(X),'+'([T])) :- completeX(Ps,X,T).


% ------------------------------------------------------------

%
% Generate shortest host (tree with hole).
% Parameter H is the nonterminal for the hole.
% Paramater V is the logic variable for the hole.
% (Use copyterm to fill into the hole.)
%

hostG(g(_,Ps),N,H,T,V)
 :-
    hostG(Ps,N,H,T,V).

hostG(Ps,N,H,n(P,T),V)
 :-
    is_list(Ps),
    findN(Ps,N,PsN),
    chooseByMindist(PsN,H,P),
    P = p(_,N,X),
    hostX(Ps,X,H,T,V).

hostX(Ps,n(N),H,T,V) :- N == H -> T = V; hostG(Ps,N,H,T,V).
hostX(Ps,s(S,X),H,s(S,T),V) :- hostX(Ps,X,H,T,V).
hostX(Ps,'?'(X),H,'?'([T]),V) :- hostX(Ps,X,H,T,V).
hostX(Ps,'*'(X),H,'*'([T]),V) :- hostX(Ps,X,H,T,V).
hostX(Ps,'+'(X),H,'+'([T]),V) :- hostX(Ps,X,H,T,V).

hostX(Ps,','(Xs),H,','(Ts),V)
 :-
    chooseByMindist(Xs,H,X),
    once(append(Xs1,[X|Xs2],Xs)),
    hostX(Ps,X,H,T,V),
    maplist(gbtf:completeX(Ps),Xs1,Ts1),
    maplist(gbtf:completeX(Ps),Xs2,Ts2),
    append(Ts1,[T|Ts2],Ts).

hostX(Ps,';'(Xs),H,';'(X,T),V)
 :-
    chooseByMindist(Xs,H,X),
    hostX(Ps,X,H,T,V).

% ------------------------------------------------------------

%
% Find all contexts for CDBC; mark them.
% Return just the marked production (as opposed to the entire grammar).
% Backtracking over this predicate gives all such marked contexts.
%

contextN(G,N,P2)
 :-
    findN(G,N,Ps),
    member(P1,Ps),
    contextP(P1,P2).

contextP(p(L,N,X1),p(L,N,X2))
 :-
    contextX(X1,X2).


%
% These are contexts.
%

contextX(n(N),{n(N)}).
contextX(';'(Xs),{';'(Xs)}).
contextX('?'(X),{'?'(X)}).
contextX('*'(X),{'*'(X)}).
contextX('+'(X),{'+'(X)}).


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
% Exercise all choices possible for the BGF expression.
% Exercise one choice point at the time.
% That is, all other choice points are completed in the shortest manner.
%

varyG(g(_,Ps),P,T)
 :-
    varyG(Ps,P,T).

varyG(Ps,P,T)
 :-
    is_list(Ps),
    varyN(Ps,P,T).

varyN(G,P,T)
 :-
    G = g(_,Ps),
    varyN(Ps,P,T).

varyN(Ps,P1,n(P2,T))
 :-
    is_list(Ps),
    P1 = p(_,_,X),
    varyX(Ps,X,T),
    unmarkG(P1,P2).

%
% Treat horizontal definitions as vertical ones.
% Inline chain productions on the fly.
%

varyX(Ps,{n(N)},n(P,T))
 :-
    findN(Ps,N,PsN),
    ( (PsN = [P], P = p(_,_,';'(Xs))) ->
        varyX(Ps,{';'(Xs)},T) 
      ; ( (PsN = [P], P = p(_,_,n(M))) ->
            varyX(Ps,{n(M)},T)
          ; (
              member(P,PsN), 
              completeP(Ps,P,T)
            )
        )
    ).

varyX(Ps,{';'(Xs)},';'(X,T))
 :-
    member(X,Xs),
    completeX(Ps,X,T).

varyX(_,{'?'(_)},'?'([])).
varyX(Ps,{'?'(X)},'?'([T])) :- completeX(Ps,X,T).
varyX(_,{'*'(_)},'*'([])).
varyX(Ps,{'*'(X)},'*'([T])) :- completeX(Ps,X,T).
varyX(Ps,{'+'(X)},'+'([T])) :- completeX(Ps,X,T).
varyX(Ps,{'+'(X)},'+'([T1,T2])) :- completeX(Ps,X,T1), completeX(Ps,X,T2).

varyX(Ps,s(S,X),s(S,T)) :- varyX(Ps,X,T).
varyX(Ps,'?'(X),'?'([T])) :- varyX(Ps,X,T).
varyX(Ps,'*'(X),'*'([T])) :- varyX(Ps,X,T).
varyX(Ps,'+'(X),'+'([T])) :- varyX(Ps,X,T).

varyX(Ps,','(Xs),','(Ts))
 :-
    append(Xs1,[X|Xs2],Xs),
    maplist(gbtf:completeX(Ps),Xs1,Ts1),
    varyX(Ps,X,T),
    maplist(gbtf:completeX(Ps),Xs2,Ts2),
    append(Ts1,[T|Ts2],Ts). 

varyX(Ps,';'(Xs),';'(X2,T))
 :-
    member(X1,Xs),
    varyX(Ps,X1,T), 
    unmarkG(X1,X2).

% ------------------------------------------------------------
