%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Map XML-based BTF representation to Prolog representation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xmlToBtf(X1,T)
 :-
    self(name(btf:tree),X1),
    child(element,X1,X2),
    xmlToT(X2,T),
    !.

xmlToT(X1,true)
 :-
    self(name(epsilon),X1),
    !.

xmlToT(X1,t(V))
 :-
    self(name(terminal),X1),
    content(X1,V),
    !.

xmlToT(X1,n(P2,T2))
 :-
    self(name(nonterminal),X1),
    child(name(bgf:production),X1,P1),
    child(name(btf:tree),X1,T1),
    xmlToP(P1,P2),
    xmlToBtf(T1,T2),
    !.

xmlToT(X1,v(string(V)))
 :-
    self(name(value),X1),
    child(name(string),X1,X2),
    !,
    content(X2,V),
    !.

xmlToT(X1,v(int(V3)))
 :-
    self(name(value),X1),
    child(name(int),X1,X2),
    !,
    content(X2,V1),
    name(V1,V2), 
    number_chars(V3,V2),
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
    child(name(btf:tree),X1,X3),
    content(X2,S),
    xmlToBtf(X3,T),
    !.

xmlToT(X1,'*'(Ts))
 :-
    self(name(star),X1),
    children(name(btf:tree),X1,Xs),
    maplist(xmlToBtf,Xs,Ts),
    !.

xmlToT(X1,'+'(Ts))
 :-
    self(name(plus),X1),
    children(name(btf:tree),X1,Xs),
    maplist(xmlToBtf,Xs,Ts),
    !.

xmlToT(X1,'?'(Ts))
 :-
    self(name(optional),X1),
    children(name(btf:tree),X1,Xs),
    maplist(xmlToBtf,Xs,Ts),
    !.

xmlToT(X1,','(Ts))
 :-
    self(name(sequence),X1),
    children(name(btf:tree),X1,Xs),
    maplist(xmlToBtf,Xs,Ts),
    !.

xmlToT(X1,';'(X,T))
 :-
    self(name(choice),X1),
    child(name(bgf:expression),X1,X2),
    child(name(btf:tree),X1,X3),
    xmlToExpression(X2,X),
    xmlToBtf(X3,T),
    !.

xmlToT(Q,_)
 :-
    Q = element(F,_,_),
    require(
      fail,
      '~q(...) is not valid BTF.',
      [F]).
