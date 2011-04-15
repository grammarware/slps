:- module(gbtf,
    [ mindepthG/1
    , distG/1
    , complete/3
    , hole/5
    , mark/3
    , vary/3
    ] ).

:- dynamic gbtf:mindepthFact/2.
:- dynamic gbtf:distFact/3.


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
    def(Ps1,N,Ps2),
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
% Determine dist between nonterminals.
% This is very similar to mindept problem.
%

distG(g(_,Ps)) 
 :-
    distG(Ps).

distG(Ps)
 :-
    is_list(Ps),
    dist1(Ps),
    distStar(Ps).


%
% Initialize dist relation with direct dependencies
% 

dist1(Ps)
 :-
      definedNs(Ps,DNs),
      member(DN,DNs),
      def(Ps,DN,PsDN),
      usedNs(PsDN,UNs),
      member(UN,UNs),
      \+ UN == DN,
      assertz(gbtf:distFact(DN,UN,1)),
      fail;
      true.


%
% Compute dists between so-far unrelated nonterminals.
% Use a helper nonterminal in between and existing relationships with it.
% Fixed point is reached if no new relationships are obtainable in this manner.
%

distStar(Ps)
 :-
    dist2(Ps,NNDs),
    ( NNDs == [] ->
        true;
        (
          member((N1,N2,D),NNDs),
          assertz(gbtf:distFact(N1,N2,D)),
          fail
        ;
          distStar(Ps)
        )
    ). 

dist2(Ps,NNDs)
 :-
    definedNs(Ps,DNs),
    findall((N1,N2,D),
      (
        member(N1,DNs),
        member(N2,DNs),
        \+ N1 == N2,
        \+ gbtf:distFact(N1,N2,_),
        findall(D12,
        (
          gbtf:distFact(N1,N3,D1),
          gbtf:distFact(N3,N2,D2),
          D12 is D1 + D2
        ),
        Ds),
        min1(Ds,D)
      ),
      NNDs).     


%
% Determine dist from a BGF expression to a nonterminal
%

distX(p(_,_,X),N,D)
 :-
    !,
    distX(X,N,D).

distX(X,N,D)
 :-
    usedNs(X,UNs),
    findall(D1,
      (
        member(UN,UNs),
        ( UN == N -> D1 = 0; gbtf:distFact(UN,N,D1) )
      ),
      Ds),
    min1(Ds,D).


%
% Given a list of options, determine the one with dist.
% The leftmost is chosen if there are multiple options with dist.
%

chooseByMindist([O1|Os],N,O)
 :-
    distX(O1,N,D1) ->
      chooseByMindist(O1,D1,Os,N,O) ;
      chooseByMindist(Os,N,O).

chooseByMindist(O,_,[],_,O).

chooseByMindist(O1,D1,[O2|Os],N,O)
 :-
    distX(O2,N,D2) ->
      ( D2 < D1 ->
          chooseByMindist(O2,D2,Os,N,O) ;
          chooseByMindist(O1,D1,Os,N,O)
      ) ;
      chooseByMindist(O1,D1,Os,N,O).


% ------------------------------------------------------------

%
% Generate shortest completion
%

complete(G,P,n(P,T))
 :-
    P = p(_,_,X),
    complete(G,X,T).

complete(G,n(N),T) :- 
    def(G,N,Ps),
    chooseByMindepth(Ps,P),
    complete(G,P,T).

complete(_,true,true).
complete(_,t(V),t(V)).
complete(G,s(S,X),s(S,T)) :- complete(G,X,T).
complete(G,','(Xs),','(Ts)) :- maplist(gbtf:complete(G),Xs,Ts).
complete(G,';'(Xs),';'(X,T)) :- chooseByMindepth(Xs,X), complete(G,X,T).
complete(_,'?'(_),'?'([])).
complete(_,'*'(_),'*'([])).
complete(G,'+'(X),'+'([T])) :- complete(G,X,T).


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

hole(_,n(N),H,V,V) :- N == H.

hole(G,n(N),H,n(P,T),V)
 :-
    \+ N == H,
    def(G,N,Ps),
    chooseByMindist(Ps,H,P),
    P = p(_,N,X),
    hole(G,X,H,T,V).

hole(G,s(S,X),H,s(S,T),V) :- hole(G,X,H,T,V).
hole(G,'?'(X),H,'?'([T]),V) :- hole(G,X,H,T,V).
hole(G,'*'(X),H,'*'([T]),V) :- hole(G,X,H,T,V).
hole(G,'+'(X),H,'+'([T]),V) :- hole(G,X,H,T,V).

hole(G,','(Xs),H,','(Ts),V)
 :-
    chooseByMindist(Xs,H,X),
    once(append(Xs1,[X|Xs2],Xs)),
    hole(G,X,H,T,V),
    maplist(gbtf:complete(G),Xs1,Ts1),
    maplist(gbtf:complete(G),Xs2,Ts2),
    append(Ts1,[T|Ts2],Ts).

hole(G,';'(Xs),H,';'(X,T),V)
 :-
    chooseByMindist(Xs,H,X),
    hole(G,X,H,T,V).


% ------------------------------------------------------------

%
% Mark all spots for BC and UC.
%

mark(C,p(L,N,X1),p(L,N,X2))
 :-
    mark(C,X1,X2).

mark(uc,n(N),{n(N)}).
mark(bc,';'(Xs),{';'(Xs)}).
mark(bc,'?'(X),{'?'(X)}).
mark(bc,'*'(X),{'*'(X)}).
mark(bc,'+'(X),{'+'(X)}).

mark(C,s(S,X1),s(S,X2)) :- mark(C,X1,X2).
mark(C,'?'(X1),'?'(X2)) :- mark(C,X1,X2).
mark(C,'*'(X1),'*'(X2)) :- mark(C,X1,X2).
mark(C,'+'(X1),'+'(X2)) :- mark(C,X1,X2).

mark(C,','(Xs1),','(Xs2))
 :-
    append(Xs1a,[X1|Xs1b],Xs1),
    append(Xs1a,[X2|Xs1b],Xs2),
    mark(C,X1,X2).

mark(C,';'(Xs1),';'(Xs2))
 :-
    append(Xs1a,[X1|Xs1b],Xs1),
    append(Xs1a,[X2|Xs1b],Xs2),
    mark(C,X1,X2).


% ------------------------------------------------------------

%
% Exercise choices for the marked BGF expression.
% There must be a marker for this predicate to succeed at all.
% All unmarked particles are completed in the shortest manner.
%

vary(G,P1,n(P2,T))
 :-
    P1 = p(_,_,X),
    vary(G,X,T),
    unmarkG(P1,P2).

%
% Treat horizontal definitions as vertical ones.
% Inline chain productions on the fly.
%

vary(G,{n(N)},n(P,T))
 :-
    def(G,N,Ps),
    ( (Ps = [P], P = p(_,_,';'(Xs))) ->
        vary(G,{';'(Xs)},T) 
      ; ( (Ps = [P], P = p(_,_,n(M))) ->
            vary(G,{n(M)},T)
          ; (
              member(P,Ps),
              P = p(_,_,X),
              complete(G,X,T)
            )
        )
    ).

vary(G,{';'(Xs)},';'(X,T))
 :-
    member(X,Xs),
    complete(G,X,T).

vary(_,{'?'(_)},'?'([])).
vary(G,{'?'(X)},'?'([T])) :- complete(G,X,T).
vary(_,{'*'(_)},'*'([])).
vary(G,{'*'(X)},'*'([T])) :- complete(G,X,T).
vary(G,{'+'(X)},'+'([T])) :- complete(G,X,T).
vary(G,{'+'(X)},'+'([T1,T2])) :- complete(G,X,T1), complete(G,X,T2).

vary(G,s(S,X),s(S,T)) :- vary(G,X,T).
vary(G,'?'(X),'?'([T])) :- vary(G,X,T).
vary(G,'*'(X),'*'([T])) :- vary(G,X,T).
vary(G,'+'(X),'+'([T])) :- vary(G,X,T).

vary(G,','(Xs),','(Ts))
 :-
    append(Xs1,[X|Xs2],Xs),
    maplist(gbtf:complete(G),Xs1,Ts1),
    vary(G,X,T),
    maplist(gbtf:complete(G),Xs2,Ts2),
    append(Ts1,[T|Ts2],Ts). 

vary(G,';'(Xs),';'(X2,T))
 :-
    member(X1,Xs),
    vary(G,X1,T), 
    unmarkG(X1,X2).


% ------------------------------------------------------------
