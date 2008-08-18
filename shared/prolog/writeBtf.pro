%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Map Prolog-based BTF representation to XML representation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tToXml(r(G,T),X)
 :-
    !,
    gToXml(G,X1),
    tToXml(T,X2),
    e(btf:root,[],[X1,X2],X).

tToXml(true,X2)
 :-
    !,
    e(epsilon,[],[],X1),
    toBtfTree(X1,X2).

tToXml(t(V),X2)
 :-
    !,
    e(terminal,[],[V],X1),
    toBtfTree(X1,X2).

tToXml(n(P,T),X4)
 :-
    !,
    pToXml(P,X1),
    tToXml(T,X2),
    e(nonterminal,[],[X1,X2],X3),
    toBtfTree(X3,X4).

tToXml(v(string(V)),X3)
 :-
    !,
    e(string,[],[V],X1),
    e(value,[],[X1],X2),
    toBtfTree(X2,X3).

tToXml(v(int(V1)),X3)
 :-
    !,
    number_chars(V1,V2),
    atom_chars(V3,V2),
    e(int,[],[V3],X1),
    e(value,[],[X1],X2),
    toBtfTree(X2,X3).

tToXml(a(Ns),X2)
 :-
    !,
    e(any,[],Ns,X1),
    toBtfTree(X1,X2).

tToXml(s(S,T),X4)
 :-
    !,
    e(selector,[],[S],X1),
    tToXml(T,X2),
    e(selectable,[],[X1,X2],X3),
    toBtfTree(X3,X4).

tToXml('*'(Ts),X2)
 :-
    !,
    maplist(tToXml,Ts,Xs),
    e(star,[],Xs,X1),
    toBtfTree(X1,X2).

tToXml('+'(Ts),X2)
 :-
    !,
    maplist(tToXml,Ts,Xs),
    e(plus,[],Xs,X1),
    toBtfTree(X1,X2).

tToXml('?'(Ts),X2)
 :-
    !,
    maplist(tToXml,Ts,Xs),
    e(optional,[],Xs,X1),
    toBtfTree(X1,X2).

tToXml(','(Ts),X2)
 :-
    !,
    maplist(tToXml,Ts,Xs),
    e(sequence,[],Xs,X1),
    toBtfTree(X1,X2).

tToXml(';'(X,T),X5)
 :-
    !,
    xToXml(X,X1),
    xToExpression(X1,X2),
    tToXml(T,X3),
    e(choice,[],[X2,X3],X4),
    toBtfTree(X4,X5).

tToXml(Q,_)
 :-
    Q =.. [F|_],
    require(
      fail,
      '~q(...) is not valid BTF.',
      [F]).

toBtfTree(X1,X2)
 :-
    e(btf:tree,[],[X1],X2).
