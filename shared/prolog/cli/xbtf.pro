:- ensure_loaded('../slps.pro').

main 
 :- 
    current_prolog_flag(argv,Argv),
    append(_,['--',BtfIn,XbgfIn,BtfOut],Argv),
    ( exists_file(BtfOut) -> delete_file(BtfOut); true ),
    load_structure(BtfIn, [T1], [dialect(xmlns)]),
    xmlToBtf(T1,T2),
    load_structure(XbgfIn, [X1], [dialect(xmlns)]),
    xml2xbgf(X1,X2),
    transformT(X2,T2,T3),
    tToXml(T3,T4),
    open(BtfOut, write, OStream),
    xml_write(OStream,T4,[]),
    close(OStream),
    halt.

:- run.
