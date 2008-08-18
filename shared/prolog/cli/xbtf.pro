:- ensure_loaded('../slps.pro').

main 
 :- 
    current_prolog_flag(argv,Argv),
    append(_,['--',BtfIn,XbgfIn,BtfOut],Argv),
    ( exists_file(BtfOut) -> delete_file(BtfOut); true ),
    load_structure(BtfIn, [BtfXmlIn], [dialect(xmlns)]),
    xmlToRoot(BtfXmlIn,r(G1,T1)),
    load_structure(XbgfIn, [XbgfXml], [dialect(xmlns)]),
    xml2xbgf(XbgfXml,Xbgf),
    transformG(Xbgf,G1,G2),
    transformT(Xbgf,T1,T2),
    tToXml(r(G2,T2),BtfXmlOut),
    open(BtfOut, write, OStream),
    xml_write(OStream,BtfXmlOut,[]),
    close(OStream),
    halt.

:- run.
