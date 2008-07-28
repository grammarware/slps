:- ensure_loaded('ll.pro').


main 
 :-
    current_prolog_flag(argv,Argv),
    append(_,['--',XsdInput,XmlInput,LtrOutput],Argv),
    loadXsd(XsdInput,SG),
    loadXml(XmlInput,Xml1),
    rootToLtr(SG,Xml1,T),
    tToXml(T,Xml2),
    open(LtrOutput, write, LtrStream),
    xml_write(LtrStream,Xml2,[]),
    close(LtrStream),
    halt.

:- run.
