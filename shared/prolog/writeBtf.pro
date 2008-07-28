%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Map Prolog-based LTR representation to XML representation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tToXml(true,X2)
 :-
    !,
    e(epsilon,[],[],X1),
    toLtrTree(X1,X2).

tToXml(n(P,T),X4)
 :-
    !,
    pToXml(P,X1),
    tToXml(T,X2),
    e(nonterminal,[],[X1,X2],X3),
    toLtrTree(X3,X4).

tToXml(a(Ns),X2)
 :-
    !,
    e(any,[],Ns,X1),
    toLtrTree(X1,X2).

tToXml(s(S,T),X4)
 :-
    !,
    e(selector,[],[S],X1),
    tToXml(T,X2),
    e(selectable,[],[X1,X2],X3),
    toLtrTree(X3,X4).

tToXml('*'(Ts),X2)
 :-
    !,
    maplist(tToXml,Ts,Xs),
    e(star,[],Xs,X1),
    toLtrTree(X1,X2).

tToXml('+'(Ts),X2)
 :-
    !,
    maplist(tToXml,Ts,Xs),
    e(plus,[],Xs,X1),
    toLtrTree(X1,X2).

tToXml('?'(Ts),X2)
 :-
    !,
    maplist(tToXml,Ts,Xs),
    e(optional,[],Xs,X1),
    toLtrTree(X1,X2).

tToXml(','(Ts),X2)
 :-
    !,
    maplist(tToXml,Ts,Xs),
    e(sequence,[],Xs,X1),
    toLtrTree(X1,X2).

tToXml(';'(X,T),X5)
 :-
    !,
    xToXml(X,X1),
    xToExpression(X1,X2),
    tToXml(T,X3),
    e(choice,[],[X2,X3],X4),
    toLtrTree(X4,X5).

tToXml(Q,_)
 :-
    Q =.. [F|_],
    require(
      fail,
      '~q(...) is not valid LTR.',
      [F]).

toLtrTree(X1,X2)
 :-
    e(ltr:tree,[],[X1],X2).
