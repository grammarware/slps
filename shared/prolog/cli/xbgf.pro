:- ensure_loaded('slps.pro').
:- ensure_loaded('xbgf0.pro').
:- use_module('xbgf1.pro').


main 
 :- 
    current_prolog_flag(argv,Argv),
    append(_,['--',BgfIn,XbgfIn,BgfOut],Argv),
    ( exists_file(BgfOut) -> delete_file(BgfOut); true ),
    load_structure(BgfIn, [G1], [dialect(xmlns)]),
    xmlToG(G1,G2),
    format(' * normalize~n',[BgfIn]), 
    load_structure(XbgfIn, [T1], [dialect(xmlns)]),
    xml2xbgf(T1,T2),
    transformG(T2,G2,G3),
    normalizeG(G3,G4),
    gToXml(G4,G5),
    open(BgfOut, write, OStream),
    xml_write(OStream,G5,[]),
    close(OStream),
    halt.

:- run.
