:- ensure_loaded('slps.pro').

main 
 :-
    current_prolog_flag(argv,Argv),
    append(_,['--',XsdInput,XmlInput,BtfOutput],Argv),
    loadXsd(XsdInput,SG),
    loadXml(XmlInput,Xml1),
    rootToBtf(SG,Xml1,T),
    tToXml(T,Xml2),
    open(BtfOutput, write, BtfStream),
    xml_write(BtfStream,Xml2,[]),
    close(BtfStream),
    halt.

:- run.
