:- ensure_loaded('../slps.pro').
% wiki: TDT


% Keep track of difference

error(1).


% Compare trees

diffT(RC,((U1,r(G1,T1)),(U2,r(G2,T2))))
 :-
    format('Diffing ~w and ~w.~n',[U1,U2]),

    % Establish equality of grammars
    G1 = g(_,Ps1),
    G2 = g(_,Ps2),
    ( (
        subset(Ps1,Ps2),
        subset(Ps2,Ps1) 
      ) -> true; 
      (
        error(RC),
        format('Declared grammars differ.~n',[])
      )
    ),

    ( diff(T1,T2,P) -> 
      (
        error(RC),
        format('Trees differ; path to first diff: ~q.~n',[P])
      )
      ; true 
    ),
    !.


main :- 
   % Compatibility hack for >6.4.1 and the use of '--'
   ( RawArgv = argv ; RawArgv = os_argv ),
   current_prolog_flag(RawArgv,Argv),
   append(_,['--'|L1],Argv),
   maplist(loadXml,L1,Xmls),
   maplist(xmlToRoot,Xmls,Ts),
   zip(L1,Ts,L2),
   findall((T1,T2),(append(_,[T1|L3],L2),append(_,[T2|_],L3)),L4),
   maplist(diffT(RC),L4),
   ( RC = 0; true ),
   halt(RC).


:- run.
