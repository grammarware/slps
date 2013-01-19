:- ensure_loaded('../slps.pro').
% wiki: DCG2BGF


dcgToG(Cs,g([],Ps))
 :-
    maplist(dcToP,Cs,Ps).

dcToP((Lhs --> Rhs),p(L,N,X))
 :-
    dcToL(Lhs,Rhs,L),
    Lhs =.. [N|_],
    rhsToX(Rhs,X).

dcToL(Lhs,_,[l(F)])
 :-
    Lhs =.. [_|Ts],
    last(Ts,T),
    nonvar(T),
    T =.. [F|_],
    \+ member(F,[',','.',[]]).

dcToL(_,Rhs,[l(F)])
 :-
    Rhs =.. [lassoc,_,_,F,_].

dcToL(_,_,[]).

rhsToX([],true).

rhsToX({_},true).

rhsToX([0' ],t(' ')).

rhsToX([0'	],t('\t')).

rhsToX([10],n('NEWLINE')).

rhsToX(R,X)
 :-
    R = (_,_),
    rhsToList(R,L),
    listToX(L,X).

rhsToX(R1,X)
 :-
    R1 =.. [F,R2|_],
    member(F,[*,+,?]),
    X =.. [F,Y],
    rhsToX(R2,Y).

rhsToX(R,t(Y))
 :-
    R =.. [F,X],
    member(F,[reserved,keyword,@]),
    name(Y,X).

rhsToX(R1,','([Y,*(','([X,Y]))]))
 :-
    R1 =.. [lassoc,R2,R3,_,_],
    rhsToX(R2,X),
    rhsToX(R3,Y).

rhsToX(T,n(N))
 :- 
    T =.. [N|_].

rhsToList({_},[]).
rhsToList(({_},R),L) :- rhsToList(R,L).
rhsToList((R1,R2),[R1|R3]) :- rhsToList(R2,R3).
rhsToList(R,[R]).

listToX([],true).
listToX([R],X) :- rhsToX(R,X).
listToX(R,','(Xs)) :- maplist(rhsToX,R,Xs).


readDcg(S,L) 
  :- 
     read(S,H),
     ( H = end_of_file ->
         L = [];
         (( H = (_ --> _) ->
           L = [H|T];
           L = T
          ), readDcg(S,T))
     ). 

:-
   current_prolog_flag(argv,Argv),
   append(_,['--',Input,Output],Argv),
   open(Input, read, IStream),
   readDcg(IStream, Dcg),
   dcgToG(Dcg,G1),
   gToXml(G1,G2),
   open(Output, write, OStream),
   xml_write(OStream,G2,[]),
   close(OStream),
   halt.
