:- ensure_loaded('../slps.pro').
% wiki: RenameAllN

casexbgf('1',all,down).
casexbgf('2',all,up).
casexbgf('3',first,down).
casexbgf('4',first,up).

mkRename(T,(X,Y),R)
 :-
    T =.. [F|Ts],
    append(Ts,[X,Y],Args),
    R =.. [F|Args].

case(Q,UD,G1,Ts) 
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


% Apply conversion

doCase(all,down,X1,X2)
 :- 
    downcase_atom(X1,X2).

doCase(all,up,X1,X2)
 :- 
    upcase_atom(X1,X2).

doCase(first,down,X1,X7) 
 :-
    name(X1,[X2|X3]),
    name(X4,[X2]),
    downcase_atom(X4,X5),
    name(X5,[X6]),
    name(X7,[X6|X3]).

doCase(first,up,X1,X7) 
 :-
    name(X1,[X2|X3]),
    name(X4,[X2]),
    upcase_atom(X4,X5),
    name(X5,[X6]),
    name(X7,[X6|X3]).


% Test a name to be worth down/up-casing

testCase(Q,UD,X) 
 :-
    doCase(Q,UD,X,Y),
    \+ X == Y.


main :- 
   current_prolog_flag(argv,Argv),
   append(_,['--',Mode,BgfFile,XbgfFile],Argv),
   require(
     casexbgf(Mode,Q,UD),
     'Unknown mode ~q.',
     [Mode]),
   loadXml(BgfFile,BgfXml),
   xmlToG(BgfXml,G),
   case(Q,UD,G,Ts),
   xbgf2xml(sequence(Ts),Xbgf),
   saveXml(XbgfFile,Xbgf),
   halt.

:- run.
