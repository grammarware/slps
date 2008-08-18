%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convenience library for XML processing %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Static namespace declarations
%

:- multifile sxmlns/2.

sxmlns(xmlns,xmlns).


%
% Use the attributes of an element to look up namespace
%

dxmlns(element(_,As,_),Pfx,Ns)
 :-
    member(xmlns:Pfx=Ns,As),
    !.


% 
% Test for absence of any local XML namespace declaration at any level 
%

noXmlnssBelow(element(_,_,Es)) :- !, maplist(noXmlnss,Es), !.
noXmlnss(element(_,As,_)) :- member(xmlns:_=_,As), !, fail.
noXmlnss(element(_,_,Es)) :- !, maplist(noXmlnss,Es), !.
noXmlnss(_).


%
%  Return content of element
%

content(element(_,[],[C]),C).


%
% Return next element and tail
%

nextElement([E|Ns],E,Ns) :- E = element(_,_,_), !.
nextElement([_|Ns1],E,Ns2) :- nextElement(Ns1,E,Ns2).


%
% The root is an element that meets a predicate
%

self((P1,P2),E) :- self(P1,E), self(P2,E).

self((P1;P2),E) :- self(P1,E); self(P2,E).

self(call(P),E) :- apply(P,[E]).

self(any,_).

self(element,element(_,_,_)).

self(name(N),element(N,_,_))
 :-
    \+ N = _:_.

self(name(Q:N),element(Ns:N,_,_))
 :-
    sxmlns(Q,Ns).


%
% Returns all the immediate children that meet a predicate
%

child(P,E,Y) :- children(P,E,[Y]).

children(P,element(_,_,Xs),Ys)
 :-
    children_filter(P,Xs,Ys).

children_filter(_,[],[]).
children_filter(P,[X|Xs],Zs) 
 :-
    ( self(P,X) -> Zs = [X|Ys]; Zs = Ys),
    children_filter(P,Xs,Ys),
    !.


%
% Return the value of an attribute
%

attribute(N,element(_,As,_),V)
 :-
    \+ N = _:_,
    member(N=V,As).

attribute(Q:N,element(_,As,_),V)
 :-
    sxmlns(Q,Ns),
    member(Ns:N=V,As).


%
% Return the value of an attribute or default if not present
%

attribute(QN,E,_,V1) :- attribute(QN,E,V2), !, V1 = V2.
attribute(_,_,D,D).


%
% Construct an element from QName, attributes, and children
%

e(Q:N,As,Es,element(Ns:N,As,Es))
 :-    
    sxmlns(Q,Ns), !.

e(N,As,Es,element(N,As,Es)).


%
% Load an XML tree from a file
%

loadXml(File,Root)
 :-
    format('Loading XML file ~q.~n',[File]),
    load_structure(File, Content, [dialect(xmlns)]),
    member(Root,Content),
    Root =.. [element|_],
    !.


%
% Save an XML tree to a file
%

saveXml(File,Root)
 :-
    format('Saving XML file ~q.~n',[File]),
    open(File, write, Stream),
    xml_write(Stream,Root,[]),
    close(Stream),
    !.


%
% Qualified names based on Pfx:Name syntax
%

qname(Pfx:N,Pfx,N)
 :-
    !.

qname(QName,Pfx,N)
 :-
    name(QName,Str),
    append(Str1,[0':|Str2],Str), % '
    name(Pfx,Str1),
    name(N,Str2),
    !.

qname2pfx(QName,Pfx)
 :-
    qname(QName,Pfx,_).

qname(X) 
 :-
    qname(X,_,_).

uqname(X) 
 :-
    \+ qname(X).

delpfx(Pfx,QN,UQN) :- qname(QN,Pfx,UQN), !.
delpfx(_,N,N).

