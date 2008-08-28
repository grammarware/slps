:- ensure_loaded('../slps.pro').


% Load a BGF file

load_bgf(Uri,G3)
 :-
    load_structure(Uri, [G1], [dialect(xmlns)]),
    format('Normalizing ~w.~n',[Uri]),
    xmlToG(G1,G2),
    normalizeG(G2,G3),
    !.

main :- 
   current_prolog_flag(argv,Argv),
   append(_,['--'|L1],Argv),
   maplist(load_bgf,L1,Gs),
   zip(L1,Gs,L2),
   findall((G1,G2),(append(_,[G1|L3],L2),append(_,[G2|_],L3)),L4),
   maplist(diffG(RC),L4),
   ( RC = 0; true ),
   halt(RC).

:- run.
