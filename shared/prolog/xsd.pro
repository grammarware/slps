%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Map XSD to Prolog-based representation of LGF %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Declare a namespace for XML Schema
%

:- multifile sxmlns/2.

sxmlns(xsd,'http://www.w3.org/2001/XMLSchema').
sxmlns(xsi,'http://www.w3.org/2001/XMLSchema-instance').


%
% Load an XSD file
%

loadXsd(File,(S,G,SGs))
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

    % Import
    children(name(xsd:import),S,Is),
    maplist(xsdImport(Dir),Is,SGs),

    % Derive grammar
    gFromSchema(S,G),
    !.


%
% Realize an import
%

xsdImport(Dir,I,SG)
 :-
    attribute(schemaLocation,I,Base),
    name(Dir,DirS),
    name(Base,BaseS),
    append(DirS,[0'/|BaseS],FileS), % '
    name(File,FileS),
    loadXsd(File,SG),
    !.


%
% Derive a grammar from a schema
%

gFromSchema(S,G2) 
 :-    
   
    % Compute roots
    children(name(xsd:element),S,Es),
    maplist(rFromElement,Es,Rs1),

    % Derive productions from global components
    children(element,S,Zs),
    maplist(psFromGlobal(S),Zs,Pss),
    concat(Pss,Ps1),

    % Check that no clashes happen
    maplist(definedN,Ps1,Ns1),
    doubles(Ns1,Ns2),
    require(
      Ns2 == [],
      'Names ~w clash after merging symbol namespaces.',
      [Ns2]),

    % Put together and normalize grammar
    G1 = g(Rs1,Ps1),
    normalizeG(G1,G2),
    !.


%
% Derive a root from a root-element declaration
%

rFromElement(E,N)
 :-
    attribute(name,E,N),
    !.


%
% Derive productions from a global XSD component
%

psFromGlobal(S,Z1,Ps)
 :-
    ( 
      self(name(xsd:element),Z1),
      !,
      pFromElement(S,Z1,P),
      Ps = [P]
    ;
      self(name(xsd:complexType),Z1),
      !,
      pFromCType(S,Z1,P),
      Ps = [P]
    ;
      self(name(xsd:group),Z1),
      !,
      pFromGroup(S,Z1,P),
      Ps = [P]
    ;
      self(name(xsd:simpleType),Z1),
      !,
      pFromSType(S,Z1,P),
      Ps = [P]
    ;
      self(name(xsd:include),Z1),
      !,
      require(
        fail,
        'XSD includes unsupported.',
        [])
    ;
      self(name(xsd:attribute),Z1),
      !,
      require(
        fail,
        'XSD attributes unsupported.',
        [])
    ;
      self(name(xsd:attributeGroup),Z1),
      !,
      require(
        fail,
        'XSD attribute groups unsupported.',
        [])
    ;
      Ps = []
    ),
    !.


%
% Derive a production from a root-element declaration
%

pFromElement(S,E,p([],N,X))
 :-
    attribute(name,E,N),
    ( 
      attribute(type,E,T1),
      normalizeQName(S,T1,T2),
      !,
      X = n(T2)
    ;
      child(name(xsd:complexType),E,T),
      !,
      xFromCType(S,T,X)
    ;
      child(name(xsd:simpleType),E,T),
      !,
      xFromSType(T,X)
    ;
      X = true
    ),
    !.


%
% Derive a production from a complex-type definition
%

pFromCType(S,T1,p([],N,X))
 :-
    attribute(name,T1,N),
    attribute(abstract,T1,false,A),
    (
      A == true,
      children(element,T1,[]),
      children(call(typeWithBase(N)),S,Ts),
      maplist(nFromType,Ts,Ns),
      X = ';'(Ns)
    ;
      A == false,
      xFromCType(S,T1,X)
    ),
    !.


%
% Derive a production from a group definition
%

pFromGroup(S,GR,p([],N,X))
 :-
    attribute(name,GR,N),
    xFromCType(S,GR,X),
    !.


%
% Derive an expression from a complex type-based content model
%

xFromCType(S,T1,X)
 :-
    (
      child(name(xsd:complexContent),T1,T2) ->
          child(name(xsd:extension),T2,T3)
        ; T3 = T1
    ),
    ( child( (name(xsd:sequence);
              name(xsd:choice);
              name(xsd:all);
              name(xsd:group)), T3, T4),
      !,
      xFromModel(S,T4,X)
    ; 
      X = true 
    ).


%
% Test the complex type to be an extension of N
%

typeWithBase(N,T)
 :- 
    self(name(xsd:complexType),T),
    children(name(xsd:complexContent),T,[T1]),
    children(name(xsd:extension),T1,[T2]),
    attribute(base,T2,N),
    !.


%
% Construct a nonterminal reference from a type
%

nFromType(T,n(N)) 
 :-
    attribute(name,T,N),
    !.


%
% Map content model to grammar expression
%

xFromModel(S,M,X)
 :-
    xFromComposite(S,sequence,',',M,X),
    !.

xFromModel(S,M,X)
 :-
    xFromComposite(S,choice,';',M,X),
    !.

xFromModel(S,M,X)
 :-
    xFromComposite(S,all,',',M,X),
    !.

xFromModel(S,M,X3)
 :-
    self(name(xsd:element),M),
    !,
    ( 
      %
      % Do not confuse s(e1,e2)* with s(e1,e2*)!
      %
      attribute(name,M,N),
      (
        attribute(type,M,T1),
        !,
        normalizeQName(S,T1,T2),
        X1 = n(T2)
      ;
        child(name(xsd:complexType),M,T),
        !,
        xFromCType(S,T,X1)
      ;
        child(name(xsd:simpleType),M,T),
        !,
        xFromSType(T,X1)
      ;
        X1 = a
      ),
      X2 = s(N,X1)
    ;
      attribute(ref,M,N1),
      normalizeQName(S,N1,N2),
      X2 = n(N2)
    ),
    xFromOccurs(M,X2,X3),
    !.

xFromModel(S,M,X)
 :-
    self(name(xsd:group),M),
    attribute(ref,M,N1),
    normalizeQName(S,N1,N2),
    xFromOccurs(M,n(N2),X),
    !.


%
% Factored commonalities of sequence, choice (and all)
%

xFromComposite(S,C,F,M,X)
 :-
    self(name(xsd:C),M),
    children(element,M,Ms),
    ( 
      Ms = [M1] -> 
        xFromModel(S,M1,X1)
      ; ( maplist(xFromModel(S),Ms,Xs), X1 =.. [F,Xs] )
    ),
    xFromOccurs(M,X1,X),
    !.


%
% Translate min/maxOccurs into regexp forms
%

xFromOccurs(M,X1,X2)
 :-
    attribute(minOccurs,M,'1',Min),
    attribute(maxOccurs,M,'1',Max1),
    once((
       Max1 == '1', Max2 = Max1
     ; Max1 == unbounded, Max2 = unbounded
     ; Max2 = unbounded
    )),
    (
       Min == '1', Max2 == '1', X2 = X1
     ; Min == '0', Max2 == '1', X2 = '?'(X1)
     ; Min == '0', Max2 == unbounded, X2 = '*'(X1)
     ; Min == '1', Max2 == unbounded, X2 = '+'(X1)
    ),
    !.


%
% Derive a production from a simple-type definition
%

pFromSType(_,T,p([],N,X))
 :-
    attribute(name,T,N),
    xFromSType(T,X).


%
% Derive an expression from a complex type-based content model
%

xFromSType(T,X)
 :-
    child(name(xsd:restriction),T,R),       
    children(name(xsd:enumeration),R,Es),
    ( \+ Es == [] ->
          (
            maplist(attribute(value),Es,Vs),
            maplist(xFromEnumeration,Vs,Xs),
            X = ';'(Xs)
          )
        ;
          (
            attribute(base,R,N),
            X = n(N)
          )
    ),
    !.

xFromEnumeration(V,s(V,true)).


%
% Normalize a qualified name
%

normalizeQName(S,N1,N2) 
 :-
    ( qname(N1,Pfx,UQN) -> 
          (
            attribute(targetNamespace,S,Tns),
            dxmlns(S,Pfx,Ns),
            ( Tns == Ns ->
                  N2 = UQN
                ; N2 = N1
            )
          )
        ; N2 = N1
    ),
    !.
