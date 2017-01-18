:- ensure_loaded('../slps.pro').
% wiki: AppendXBGF


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
   % Compatibility hack for >6.4.1 and the use of '--'
   ( RawArgv = argv ; RawArgv = os_argv ),
   current_prolog_flag(RawArgv,Argv),
   append(_,['--',Out|Ins],Argv),
   maplist(loadSequence,Ins,Xmlss),
   concat(Xmlss,Xmls),
   e(xbgf:sequence,[],Xmls,Root),
   saveXml(Out,Root),
   halt.

:- run.
