%
% Static namespace declarations
%
% wiki: WriteXBGF

:- multifile sxmlns/2.

sxmlns(xbgf,'http://planet-sl.org/xbgf').

xbgf2xml(sequence(Ts),Xml)
 :-
    maplist(xbgf2xml,Ts,Xmls),
    e(xbgf:sequence,[],Xmls,Xml).

xbgf2xml(renameL(L1,L2),Xml)
 :-
    e(from,[],[L1],Xml1),
    e(to,[],[L2],Xml2),
    e(label,[],[Xml1,Xml2],Xml3),
    e(xbgf:rename,[],[Xml3],Xml).

xbgf2xml(renameN(N1,N2),Xml)
 :-
    e(from,[],[N1],Xml1),
    e(to,[],[N2],Xml2),
    e(nonterminal,[],[Xml1,Xml2],Xml3),
    e(xbgf:rename,[],[Xml3],Xml).

xbgf2xml(renameS([],S1,S2),Xml)
 :-
    e(from,[],[S1],Xml1),
    e(to,[],[S2],Xml2),
    e(selector,[],[Xml1,Xml2],Xml3),
    e(xbgf:rename,[],[Xml3],Xml).

xbgf2xml(anonymize(P1),Xml)
 :-
    pToXml(P1,P2),
    e(xbgf:anonymize,[],[P2],Xml).

xbgf2xml(abstractize(P1),Xml)
 :-
    pToXml(P1,P2),
    e(xbgf:abstractize,[],[P2],Xml).
