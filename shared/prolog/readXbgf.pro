%
% Static namespace declarations
%

:- multifile sxmlns/2.

sxmlns(xbgf,'http://planet-sl.org/xbgf').


%
% Convert XML to predicates
%

xml2xbgf(T,addV(P2))
 :-
    self(name(xbgf:add),T),
    child(name(vertical),T,T1),
    child(name(bgf:production),T1,P1),
    xmlToP(P1,P2).

xml2xbgf(T,addH(P2))
 :-
    self(name(xbgf:add),T),
    child(name(horizontal),T,T1),
    child(name(bgf:production),T1,P1),
    xmlToP(P1,P2).

xml2xbgf(T,chain(P2))
 :-
    self(name(xbgf:chain),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,define(Ps2))
 :-
    self(name(xbgf:define),T),
    children(name(bgf:production),T,Ps1),
    maplist(xmlToP,Ps1,Ps2).

xml2xbgf(T,designate(P2))
 :-
    self(name(xbgf:designate),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,appear(P2))
 :-
    self(name(xbgf:appear),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,disappear(P2))
 :-
    self(name(xbgf:disappear),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,unlabel(L))
 :-
    self(name(xbgf:unlabel),T),
    child(name(label),T,T1),
    content(T1,L).

xml2xbgf(T,deyaccify(N))
 :-
    self(name(xbgf:deyaccify),T),
    content(T,N).

xml2xbgf(T,abstractize(P2))
 :-
    self(name(xbgf:abstractize),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,concretize(P2))
 :-
    self(name(xbgf:concretize),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,anonymize(P2))
 :-
    self(name(xbgf:anonymize),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,deanonymize(P2))
 :-
    self(name(xbgf:deanonymize),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,distributeL(L))
 :-
    self(name(xbgf:distribute),T),
    child(name(label),T,T1),
    content(T1,L).

xml2xbgf(T,distributeN(N))
 :-
    self(name(xbgf:distribute),T),
    child(name(nonterminal),T,T1),
    content(T1,N).

xml2xbgf(T,downgrade(P1,P2))
 :-
    self(name(xbgf:downgrade),T),
    children(name(bgf:production),T,[P3,P4]),
    xmlToP(P3,P1),
    xmlToP(P4,P2).

xml2xbgf(T,dump)
 :-
    self(name(xbgf:dump),T).

xml2xbgf(T,eliminate(N))
 :-
    self(name(xbgf:eliminate),T),
    content(T,N).

xml2xbgf(T,G)
 :-
    self(name(xbgf:extract),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2),
    inScope2xbgf(extract,extractL,extractN,[P2],T,G).

xml2xbgf(T,G)
 :-
    self(name(xbgf:factor),T),
    children(name(bgf:expression),T,[T1,T2]),
    xmlToExpression(T1,X1),
    xmlToExpression(T2,X2),
    inScope2xbgf(factor,factorL,factorN,[X1,X2],T,G).

xml2xbgf(T,factorN(N))
 :-
    self(name(xbgf:factor),T),
    child(name(nonterminal),T,T1),
    content(T1,N).

xml2xbgf(T,G)
 :-
    self(name(xbgf:fold),T),
    child(name(nonterminal),T,T1),
    content(T1,N),
    inScope2xbgf(fold,foldL,foldN,[N],T,G).

xml2xbgf(T,horizontal(N))
 :-
    self(name(xbgf:horizontal),T),
    content(T,N).

xml2xbgf(T,id)
 :-
    self(name(xbgf:id),T).

xml2xbgf(T,inject(P2))
 :-
    self(name(xbgf:inject),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,inline(N))
 :-
    self(name(xbgf:inline),T),
    content(T,N).

xml2xbgf(T,introduce(Ps2))
 :-
    self(name(xbgf:introduce),T),
    children(name(bgf:production),T,Ps1),
    maplist(xmlToP,Ps1,Ps2).

xml2xbgf(T,import(Ps2))
 :-
    self(name(xbgf:import),T),
    children(name(bgf:production),T,Ps1),
    maplist(xmlToP,Ps1,Ps2).

xml2xbgf(T,lassoc(P2))
 :-
    self(name(xbgf:lassoc),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,G)
 :-
    self(name(xbgf:massage),T),
    children(name(bgf:expression),T,[T1,T2]),
    xmlToExpression(T1,X1),
    xmlToExpression(T2,X2),
    inScope2xbgf(massage,massageL,massageN,[X1,X2],T,G).

xml2xbgf(T,G)
 :-
    self(name(xbgf:narrow),T),
    children(name(bgf:expression),T,[T1,T2]),
    xmlToExpression(T1,X1),
    xmlToExpression(T2,X2),
    inScope2xbgf(narrow,narrowL,narrowN,[X1,X2],T,G).

xml2xbgf(T,permute(P2))
 :-
    self(name(xbgf:permute),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,project(P2))
 :-
    self(name(xbgf:project),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,rassoc(P2))
 :-
    self(name(xbgf:rassoc),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,removeV(P2))
 :-
    self(name(xbgf:remove),T),
    child(name(vertical),T,T1),
    child(name(bgf:production),T1,P1),
    xmlToP(P1,P2).

xml2xbgf(T,removeH(P2))
 :-
    self(name(xbgf:remove),T),
    child(name(horizontal),T,T1),
    child(name(bgf:production),T1,P1),
    xmlToP(P1,P2).

xml2xbgf(T,F3)
 :-
    self(name(xbgf:rename),T),
    (
      child(name(label),T,X),
      F = renameL,
      C = label
    ;
      child(name(nonterminal),T,X),
      F = renameN,
      C = nonterminal
    ;
      child(name(selector),T,X),
      (
        child(name(in),X,In) ->
            ( 
              content(In,Z0),
              F = renameS([Z0])
            )
          ;
            F = renameS([])
      ),
      C = selector
    ;
      child(name(terminal),T,X),
      F = renameT,
      C = terminal
    ),
    child(name(from),X,From),
    child(name(to),X,To),
    content(From,Z1),
    content(To,Z2),
    F =.. F1,
    append(F1,[Z1,Z2],F2),
    F3 =.. F2.

xml2xbgf(T,G)
 :-
    self(name(xbgf:replace),T),
    children(name(bgf:expression),T,[T1,T2]),
    xmlToExpression(T1,X1),
    xmlToExpression(T2,X2),
    inScope2xbgf(replace,replaceL,replaceN,[X1,X2],T,G).

xml2xbgf(T,reroot(Rs2))
 :-
    self(name(xbgf:reroot),T),
    children(name(root),T,Rs1),
    maplist(content,Rs1,Rs2).

xml2xbgf(T,sequence(Ts2))
 :-
    self(name(xbgf:sequence),T),
    children(element,T,Ts1),
    !,
    maplist(once(xml2xbgf),Ts1,Ts2).

xml2xbgf(T,abridge(P2))
 :-
    self(name(xbgf:abridge),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,stripL(L))
 :-
    self(name(xbgf:strip),T),
    child(name(label),T,T1),
    content(T1,L).

xml2xbgf(T,stripLs)
 :-
    self(name(xbgf:strip),T),
    child(name(allLabels),T,_).

xml2xbgf(T,stripS(S))
 :-
    self(name(xbgf:strip),T),
    child(name(selector),T,T1),
    content(T1,S).

xml2xbgf(T,stripSs)
 :-
    self(name(xbgf:strip),T),
    child(name(allSelectors),T,_).

xml2xbgf(T,stripT(T2))
 :-
    self(name(xbgf:strip),T),
    child(name(terminal),T,T1),
    content(T1,T2).

xml2xbgf(T,stripTs)
 :-
    self(name(xbgf:strip),T),
    child(name(allTerminals),T,_).

xml2xbgf(T,unchain(P2))
 :-
    self(name(xbgf:unchain),T),
    child(name(bgf:production),T,P1),
    xmlToP(P1,P2).

xml2xbgf(T,undefine(N))
 :-
    self(name(xbgf:undefine),T),
    content(T,N).

xml2xbgf(T,redefine(Ps2))
 :-
    self(name(xbgf:redefine),T),
    children(name(bgf:production),T,Ps1),
    maplist(xmlToP,Ps1,Ps2).

xml2xbgf(T,G)
 :-
    self(name(xbgf:unfold),T),
    child(name(nonterminal),T,T1),
    content(T1,N),
    inScope2xbgf(unfold,unfoldL,unfoldN,[N],T,G).

xml2xbgf(T,unite(N1,N2))
 :-
    self(name(xbgf:unite),T),
    child(name(add),T,Add),
    child(name(to),T,To),
    content(Add,N1),
    content(To,N2).

xml2xbgf(T,upgrade(P1,P2))
 :-
    self(name(xbgf:upgrade),T),
    children(name(bgf:production),T,[P3,P4]),
    xmlToP(P3,P1),
    xmlToP(P4,P2).

xml2xbgf(T,verticalL(L))
 :-
    self(name(xbgf:vertical),T),
    child(name(label),T,T1),
    content(T1,L).

xml2xbgf(T,verticalN(N))
 :-
    self(name(xbgf:vertical),T),
    child(name(nonterminal),T,T1),
    content(T1,N).

xml2xbgf(T,yaccify(Ps2))
 :-
    self(name(xbgf:yaccify),T),
    children(name(bgf:production),T,Ps1),
    maplist(xmlToP,Ps1,Ps2).

xml2xbgf(T,G)
 :-
    self(name(xbgf:widen),T),
    children(name(bgf:expression),T,[T1,T2]),
    xmlToExpression(T1,X1),
    xmlToExpression(T2,X2),
    inScope2xbgf(widen,widenL,widenN,[X1,X2],T,G).

xml2xbgf(T,_)
 :-
    T = element(N,_,_),
    format(
      'Cannot read XML tree ~q(...).~n',
      [N]),
    !,
    fail.

inScope2xbgf(None,L,N,Args,T,G)
 :-
    ( child(name(in),T,T1) -> 
      ( 
        child(name(label),T1,T2),
        content(T2,C),
        append(Args,[C],Args1),
        G =.. [L|Args1]
      ;
        child(name(nonterminal),T1,T2),
        content(T2,C),
        append(Args,[C],Args1),
        G =.. [N|Args1]
      )
    ;
      G =.. [None|Args]
    ).
