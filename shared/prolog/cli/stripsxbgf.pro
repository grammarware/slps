:- ensure_loaded('../slps.pro').
% wiki: RetireSs

strips(p(As,N,X1),[anonymize(p(As,N,X2))]) 
 :-
    transform(try(strips_rule),X1,X2),
    \+ X1 == X2,
    !.

strips(_,[]).

strips_rule(X,{X}) :- X = s(_,_).

main :- 
   % Compatibility hack for >6.4.1 and the use of '--'
   ( RawArgv = argv ; RawArgv = os_argv ),
   current_prolog_flag(RawArgv,Argv),
   append(_,['--',BgfFile,XbgfFile],Argv),
   loadXml(BgfFile,BgfXml),
   xmlToG(BgfXml,g(_,Ps)),
   maplist(strips,Ps,Tss),
   concat(Tss,Ts),
   xbgf2xml(sequence(Ts),Xbgf),
   saveXml(XbgfFile,Xbgf),
   halt.

:- run.
