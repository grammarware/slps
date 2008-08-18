:- ensure_loaded('../slps.pro').


%
% Static namespace declarations
%

:- multifile sxmlns/2.

sxmlns(xbgf,'http://planet-sl.org/xbgf').


main :- 
   current_prolog_flag(argv,Argv),
   append(_,['--',Out|Ins],Argv),
   maplist(loadXml,Ins,Xmls),
   e(xbgf:sequence,[],Xmls,Root),
   saveXml(Out,Root),
   halt.

:- run.
