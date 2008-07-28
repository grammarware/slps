%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Languedoc Tree Representation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Static namespace declarations 
%

:- multifile sxmlns/2.

sxmlns(ltr,'http://planet-sl.org/ltr').


%
% Parse a root
%

rootToLtr(SG,E,n(P,T))
 :-
    require(
      (
        SG = (S,G,_),
        G = g(_,Ps),
        attribute(targetNamespace,S,Tns),
        E = element(QN,_,_),
        QN = Tns:N,
        children(name(xsd:element),S,EDs),
        member(ED,EDs),
        attribute(name,ED,N),
        splitN(Ps,N,[P],_,_),
        P = p(_,_,X)
      ),
      'No root determined,',
      []),
    eToLtr(SG,QN,X,E,T),
    !.


%
% Parse an entire element
%

eToLtr(SG,N1,X1,E,T3)
 :-
    (
      attribute(xsi:type,E,V) -> 
          (
            require(
              X1 = n(N2),
              'Element ~q with xsi:type=~q is of non-nominal type ~q.',
              [N1,V,X1]),
            SG = (_,G,_),
            G = g(_,Ps),
            splitN(Ps,N2,[P],_,_),
            P = p(_,_,X2),
            (
              X2 = ';'(Xs) ->
                  true
                ; [X2] = Xs
            ),
            Y = n(V),
            require(
              member(Y,Xs),
              'Choice ~q must cover xsi:type=~q.',
              [X2,V]),
            (
              X2 = ';'(Xs) ->
                  T2 = ';'(Y,T1) 
                ; T2 = T1
            ),
            T3 = n(P,T2)
          )
        ;
          (
            Y = X1,
            T3 = T1
          )
    ),
    E = element(_,_,Ns1),
    require(
      xToLtr(SG,Y,Ns1,Ns2,T1),
      'Cannot parse element ~q according to ~q.',
      [N1,Y]),
    require(
      \+ nextElement(Ns2,_,_),
      'Element ~q parsed incompletely as ~q according to ~q.',
      [N1,T1,Y]).


%
% Parse nodes according to an expression
%

xToLtr(_,true,Ns,Ns,true)
 :-
    !.

xToLtr(SG,n(N),Ns1,Ns2,n(P,T))
 :-
    findGlobal(SG,N,element,_),
    !,
    SG = (S,G,_),
    attribute(targetNamespace,S,Tns),
    nextElement(Ns1,E,Ns2),
    E = element(QN,_,_),
    QN = Tns:N,
    G = g(_,Ps),
    splitN(Ps,N,[P],_,_),
    P = p(_,_,X),
    eToLtr(SG,N,X,E,T),
    !.

xToLtr(SG,n(N),Ns1,Ns2,n(P,T))
 :-
    findGlobal(SG,N,group,_),
    !,
    SG = (_,G,_),
    G = g(_,Ps),
    splitN(Ps,N,[P],_,_),
    P = p(_,_,X),
    xToLtr(SG,X,Ns1,Ns2,T),
    !.

xToLtr(SG,n(N),Ns1,Ns2,n(P,T))
 :-
    findGlobal(SG,N,complexType,_),
    !,
    SG = (_,G,_),
    G = g(_,Ps),
    splitN(Ps,N,[P],_,_),
    P = p(_,_,X),
    xToLtr(SG,X,Ns1,Ns2,T),
    !.

xToLtr(SG,n(N),Ns,[],T)
 :-
    findGlobal(SG,N,simpleType,_),
    !,
    simpleType(N,Ns,T),
    SG = (_,G,_),
    G = g(_,Ps),
    splitN(Ps,N,[_],_,_),
    !.

xToLtr(SG,n(QN),Ns1,Ns2,T)
 :-
    qname(QN,Pfx,N),
    SG = (S,_,SGs),
    dxmlns(S,Pfx,Ns),
    ( 
      sxmlns(xsd,Ns) ->
          ( require(
              simpleXsd(N),
              'Cannot handle symbol ~q.',
              [QN]),
            simpleType(QN,Ns1,T),
            Ns2 = []
          )
        ; (
            member(SG1,SGs),
            SG1 = (S1,_),
            attribute(targetNamespace,S1,Ns),
            !,
            xToLtr(SG1,n(N),Ns1,Ns2,T)
          )
    ),
    !.

xToLtr(SG,s(Sel,X),Ns1,Ns2,s(Sel,T))
 :-
    !,
    nextElement(Ns1,E,Ns2),
    E = element(N,_,_),
    SG = (S,_,_),
    attribute(elementFormDefault,S,unqualified,EFD),
    once(( 
        (
          EFD = unqualified,
          N == Sel
        )
      ;
        ( 
          EFD = qualified,
          attribute(targetNamespace,S,Tns),
          N = Tns:Sel
        ) 
    )),
    eToLtr(SG,Sel,X,E,T),
    !.

xToLtr(SG,','(Xs),Es1,Es2,','(Ts))
 :-
    !,
    accum(xToLtr(SG),Xs,Es1,Es2,Ts),
    !.

xToLtr(SG,';'(Xs),Es1,Es2,';'(X,T))
 :-
    !,
    member(X,Xs),
    xToLtr(SG,X,Es1,Es2,T),
    !.

xToLtr(SG,'*'(X),Es1,Es2,'*'(Ts))
 :-
    !,
    manyToLtr(SG,X,Es1,Es2,Ts),
    !.

xToLtr(SG,'+'(X),Es1,Es2,'+'(Ts))
 :-
    !,
    many1ToLtr(SG,X,Es1,Es2,Ts),
    !.

xToLtr(SG,'?'(X),Es1,Es2,'?'(Ts))
 :-
    !,
    optionalToLtr(SG,X,Es1,Es2,Ts),
    !.


%
% Find a global by name and category
% 

findGlobal(SG,N,C,Gl)
 :-
    uqname(N),
    SG = (S,_,_),
    children(name(xsd:C),S,Gls),
    member(Gl,Gls),
    attribute(name,Gl,N),
    !.


%
% Parse according to occurrence constraints
%

many1ToLtr(SG,X,Es1,Es3,[T|Ts])
 :-
    xToLtr(SG,X,Es1,Es2,T),
    !,
    manyToLtr(SG,X,Es2,Es3,Ts),
    !.

manyToLtr(SG,X,Es1,Es3,[T|Ts])
 :-
    xToLtr(SG,X,Es1,Es2,T),
    !,
    manyToLtr(SG,X,Es2,Es3,Ts),
    !.

manyToLtr(_,_,Es,Es,[])
 :-
    !.

optionalToLtr(SG,X,Es1,Es2,[T])
 :-
    xToLtr(SG,X,Es1,Es2,T),
    !.

optionalToLtr(_,_,Es,Es,[])
 :-
    !.


%
% "Supported" simple XSD types
%

simpleXsd(int).
simpleXsd(string).


%
% Parse according to a simple type
%

simpleType(N,Ns,a(Ns))
 :-
    require(
      \+ nextElement(Ns,_,_),
      'Simple type ~q expected; elements found.',
      [N]).


%
% Pretty printer
%

ppT(T1) 
 :-
    condenseT(T1,T2),
    writeq(T2), nl.

% indent(0) :- !.
% indent(N1) :- N1 > 0, write(' '), N2 is N1 - 1, indent(N2).


condenseT(true,true)
 :-
    !.

condenseT(n(P1,T1),P2)
 :-
    P1 = p(As,_,_),
    member(l(L),As),
    !,
    condenseT(T1,T2),
    P2 =.. [L,T2],
    !. 

condenseT(n(P1,T1),P2)
 :-
    P1 = p(_,N,_),
    !,
    condenseT(T1,T2),
    P2 =.. [N,T2],
    !. 

condenseT(X,X)
 :-
    X = a(_),
    !.

condenseT(s(S,T1),T3)
 :-
    !,
    condenseT(T1,T2),
    T3 =.. [S,T2],
    !.

condenseT('*'(Ts1),Ts2)
 :-
    !,
    maplist(condenseT,Ts1,Ts2),
    !.

condenseT('+'(Ts1),Ts2)
 :-
    !,
    maplist(condenseT,Ts1,Ts2),
    !.

condenseT('?'(Ts1),Ts2)
 :-
    !,
    maplist(condenseT,Ts1,Ts2),
    !.

condenseT(','(Ts1),Ts2)
 :-
    !,
    maplist(condenseT,Ts1,Ts2),
    !.

condenseT(';'(_,T1),T2)
 :-
    !,
    condenseT(T1,T2),
    !.
