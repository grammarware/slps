%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Map XML-based LTR representation to Prolog representation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xmlToLtr(X1,T)
 :-
    self(name(ltr:tree),X1),
    child(element,X1,X2),
    xmlToT(X2,T),
    !.

xmlToT(X1,true)
 :-
    self(name(epsilon),X1),
    !.

xmlToT(X1,n(P2,T2))
 :-
    self(name(nonterminal),X1),
    child(name(lgf:production),X1,P1),
    child(name(ltr:tree),X1,T1),
    xmlToP(P1,P2),
    xmlToLtr(T1,T2),
    !.

xmlToT(X1,a(Ns))
 :-
    self(name(any),X1),
    children(any,X1,Ns),
    !.

xmlToT(X1,s(S,T))
 :-
    self(name(selectable),X1),
    child(name(selector),X1,X2),
    child(name(ltr:tree),X1,X3),
    content(X2,S),
    xmlToLtr(X3,T),
    !.

xmlToT(X1,'*'(Ts))
 :-
    self(name(star),X1),
    children(name(ltr:tree),X1,Xs),
    maplist(xmlToLtr,Xs,Ts),
    !.

xmlToT(X1,'+'(Ts))
 :-
    self(name(plus),X1),
    children(name(ltr:tree),X1,Xs),
    maplist(xmlToLtr,Xs,Ts),
    !.

xmlToT(X1,'?'(Ts))
 :-
    self(name(optional),X1),
    children(name(ltr:tree),X1,Xs),
    maplist(xmlToLtr,Xs,Ts),
    !.

xmlToT(X1,','(Ts))
 :-
    self(name(sequence),X1),
    children(name(ltr:tree),X1,Xs),
    maplist(xmlToLtr,Xs,Ts),
    !.

xmlToT(X1,';'(X,T))
 :-
    self(name(choice),X1),
    child(name(lgf:expression),X1,X2),
    child(name(ltr:tree),X1,X3),
    xmlToExpression(X2,X),
    xmlToLtr(X3,T),
    !.

xmlToT(Q,_)
 :-
    Q = element(F,_,_),
    require(
      fail,
      '~q(...) is not valid LTR.',
      [F]).
