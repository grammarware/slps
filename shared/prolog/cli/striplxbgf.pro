:- ensure_loaded('../slps.pro').

mkUnlabel(T,(X,Y),R)
 :-
    T =.. [F|Ts],
    append(Ts,[X,Y],Args),
    R =.. [F|Args].

stripl(G1,Ts) 
 :-
    % Handle nonterminals

    allNs(G1,Ns1),
    filter(testCase(Q,UD),Ns1,Ns2),
    maplist(doCase(Q,UD),Ns2,Ns3),
    zip(Ns2,Ns3,Ns4),
    maplist(mkRename(renameN),Ns4,Ts1),

    % Handle labels

    allLs(G1,Ls1),
    filter(testCase(Q,UD),Ls1,Ls2),
    maplist(doCase(Q,UD),Ls2,Ls3),
    zip(Ls2,Ls3,Ls4),
    maplist(mkRename(renameL),Ls4,Ts2),

    % Handle selectors

    allSs(G1,Ss1),
    filter(testCase(Q,UD),Ss1,Ss2),
    maplist(doCase(Q,UD),Ss2,Ss3),
    zip(Ss2,Ss3,Ss4),
    maplist(mkRename(renameS([])),Ss4,Ts3),
  
    concat([Ts1,Ts2,Ts3],Ts).


main :- 
   current_prolog_flag(argv,Argv),
   append(_,['--',BgfFile,XbgfFile],Argv),
   loadXml(BgfFile,BgfXml),
   xmlToG(BgfXml,g(_,Ps)),
   stripl(G,Ts),
   xbgf2xml(sequence(Ts),Xbgf),
   saveXml(XbgfFile,Xbgf),
   halt.

:- run.
