:- ensure_loaded('../slps.pro').
% wiki: XSD2BGF

loadOneXsd(File,(S,G,_))
 :-
    % Load schema and check schema element
    format('Loading XSD file ~q.~n',[File]),
    load_structure(File, Content, [dialect(xmlns)]),
    member(S,Content),
    S =.. [element|_],
    self(name(xsd:schema),S),
    noXmlnssBelow(S),

    % Derive dir of schema file
    ( 
      name(File,FileS),
      append(DirS,[0'/|BaseS],FileS),
      \+ member(0'/,BaseS),
      name(Dir,DirS),
      !
    ;
      Dir = '.'
    ),

    % Derive grammar
    gFromSchema(S,G),
    !.

main 
 :-
    current_prolog_flag(argv,Argv),
    append(_,['--',XsdFile,BgfFile],Argv),
    loadOneXsd(XsdFile,G1),
    completeXsd(G1,G2),
    gToXml(G2,Xml),
    saveXml(BgfFile,Xml),
    halt.

:- run.
