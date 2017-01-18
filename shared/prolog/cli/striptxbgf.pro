:- ensure_loaded('../slps.pro').
% wiki: RetireTs

stript(p(As,N,X1),[abstractize(p(As,N,X2))]) 
 :-
    transform(try(stript_rule),X1,X2),
    \+ X1 == X2,
    !.

stript(_,[]).

stript_rule(X,{X}) :- X = t(_).

main :- 
   % Compatibility hack for >6.4.1 and the use of '--'
   ( RawArgv = argv ; RawArgv = os_argv ),
   current_prolog_flag(RawArgv,Argv),
   append(_,['--',BgfFile,XbgfFile],Argv),
   loadXml(BgfFile,BgfXml),
   xmlToG(BgfXml,g(_,Ps)),
   maplist(stript,Ps,Tss),
   concat(Tss,Ts),
   xbgf2xml(sequence(Ts),Xbgf),
   saveXml(XbgfFile,Xbgf),
   halt.

:- run.
