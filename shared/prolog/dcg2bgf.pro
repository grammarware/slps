:- ensure_loaded('slps.pro').


gFromDcg(Cs,g([],Ps))
 :-
    maplist(pFromClause,Cs,Ps).

pFromClause((Lhs --> Rhs),p(As2,N,X))
 :-
    xFromRhs(As1,Rhs,X),
    Lhs =.. [N|Args],
    ( ( last(Args,Arg),
        nonvar(Arg),
        Arg =.. [F|_],
        \+ member(F,[',','.',[],';'])
      ) ->
          As2 = [l(F)]
        ; As2 = As1
    ),
    !.

xFromRhs([],[],true)
 :- 
    !.

xFromRhs([],{_},true)
 :-
    !.

xFromRhs([],Rhs,X)
 :-
    Rhs = (_,_),
    !,
    rhsToList(Rhs,L),
    xFromList(L,X).

xFromRhs([],Rhs,'+'(X))
 :-
    Rhs =.. [many1,Arg|_],
    !,
    xFromRhs(_,Arg,X).

xFromRhs([],Rhs,'*'(X))
 :-
    Rhs =.. [many,Arg|_],
    !,
    xFromRhs(_,Arg,X).

xFromRhs([],Rhs,t(Y))
 :-
    Rhs =.. [F,X],
    member(F,[keyword,special]),
    !,
    name(Y,X).

xFromRhs([l(F)],Rhs,','([Y,'*'(','([X,Y]))]))
 :-
    Rhs =.. [lassoc,Ops,Arg,F,_],
    !,
    xFromRhs(_,Ops,X),
    xFromRhs(_,Arg,Y).

xFromRhs(_,T,n(N))
 :- 
    T =.. [N|_].


rhsToList({_},[]) :- !.
rhsToList(({_},Rhs),L) :- !, rhsToList(Rhs,L).
rhsToList((A,B),[A|R]) :- !, rhsToList(B,R).
rhsToList(Rhs,[Rhs]) :- !.

xFromList([],true) :- !.
xFromList([Rhs],X) :- !, xFromRhs(_,Rhs,X).
xFromList(Rhs,','(Xs)) :- !, maplist(xFromRhs(_),Rhs,Xs).


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
   gFromDcg(Dcg,G1),
   gToXml(G1,G2),
   open(Output, write, OStream),
   xml_write(OStream,G2,[]),
   close(OStream),
   halt.
