:- ensure_loaded('../slps.pro').


%
% Static namespace declarations
%

:- multifile sxmlns/2.

sxmlns(xbgf,'http://planet-sl.org/xbgf').

loadSequence(In,Xmls)
 :-
    loadXml(In,Xml),
    children(element,Xml,Xmls),
    !.

main :- 
   current_prolog_flag(argv,Argv),
   append(_,['--',Out|Ins],Argv),
   maplist(loadSequence,Ins,Xmlss),
   concat(Xmlss,Xmls),
   e(xbgf:sequence,[],Xmls,Root),
   saveXml(Out,Root),
   halt.

:- run.
