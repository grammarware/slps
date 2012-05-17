% -*- Prolog -*-

:- set_prolog_flag(double_quotes, string).


main(FileName) :-
	trace_from_file(FileName, Trace),
	jdom:build(_, Result, Trace, []),
	writeq(Result).

mapped(FileName) :-
	trace_from_file(FileName, Trace),
	jdom:build(_, Result, Trace, []),
	mapped_from_mapping(Result, Mapped),
	writeq(Mapped).

mapped_from_mapping([], []).
mapped_from_mapping([(_From, To)|Tail], Mapped) :-
	mapped_from_mapping(Tail, TailMapped),
	append(To, TailMapped, Mapped).


trace_from_file(Filename, Trace) :-
	 open(Filename, read, Stream, []), 
	 read(Stream, Trace), 
	 close(Stream).


test(Doc, Result) :- 
	new_jdom_trace(Trace), 
	jdom:build(Doc, Result, Trace, []),
	print_jdom_dom_list(Result).

%% TODO: test that the generated trace conforms to DOM grammar.

jdom:build(Doc, [(JDomNewDoc, DomCreate), (JDomNewRoot, DomObtain), (JDomAddContent, [])|DomChildren])
-->
	jdom:newDocument(Doc, JDomNewDoc),
	jdom:newElement(Root, JDomNewRoot),
	jdom:addContentToDoc(Doc, Root, JDomAddContent),
	jdom:content(Root, Doc, DomChildren),

	{dom:createDocument(Doc, DomCreate),
	dom:obtainRoot(Doc, Root, DomObtain)}.



jdom:content(Element, Doc, [(SetText,DomSetText)|DomChildren]) 
-->
	jdom:setText(Element, SetText),
	jdom:content(Element, Doc, DomChildren),

	{dom:createTextNode(Doc, Child, DomNode),
	dom:appendChild(Element, Child, DomAppend),
	append(DomNode, DomAppend, DomSetText)}.

jdom:content(Element, Doc, Result) 
-->
	jdom:newElement(Child, NewElement),
	jdom:content(Child, Doc, DomChildChildren),
	jdom:addContent(Element, Child, AddContent),
	jdom:content(Element, Doc, DomChildren),

	% Note how DomChildChildren is put at the right location.
	{dom:createElement(Doc, Child, DomCreateChild),
	dom:appendChild(Element, Child, DomAppend),
	append([(NewElement, DomCreateChild)|DomChildChildren], [(AddContent,DomAppend)], Prefix),
	append(Prefix, DomChildren, Result)}.

jdom:content(_Element, _Doc, []) --> [].



%% DOM non terminal generators

dom:createDocument(Doc, [MakeFactory, MakeBuilder, MakeImpl, MakeDoc]) 
:-
	MakeFactory = static(Factory, '<javax.xml.parsers.DocumentBuilderFactory: javax.xml.parsers.DocumentBuilderFactory newInstance()>', []),
	MakeBuilder = interface(Builder, '<javax.xml.parsers.DocumentBuilderFactory: javax.xml.parsers.DocumentBuilder newDocumentBuilder()>', [Factory]),
	MakeImpl = interface(DomImpl, '<javax.xml.parsers.DocumentBuilder: org.w3c.dom.DOMImplementation getDOMImplementation()>', [Builder]), 
	MakeDoc = virtual(Doc, '<org.w3c.dom.DOMImplementation: org.w3c.dom.Document createDocument(java.lang.String,java.lang.String,org.w3c.dom.DocumentType)>', [DomImpl, _, _, _]).

dom:obtainRoot(Doc, Root, DomObtain) 
:-
	DomObtain = [virtual(Root, '<org.w3c.dom.Document: org.w3c.dom.Element getDocumentElement()>', [Doc])].

dom:createElement(Doc, Elt, DomCreateElement) 
:-
	DomCreateElement = [virtual(Elt, '<org.w3c.dom.Document: org.w3c.dom.Element createElement(java.lang.String)>', [Doc, _String])].

dom:createTextNode(Doc, Node, DomCreateNode)
:-
	DomCreateNode = [virtual(Node, '<org.w3c.dom.Document: org.w3c.dom.Text createTextNode(java.lang.String)>', [Doc, _String])].


dom:appendChild(Element, Child, DomAppend) 
:-
	DomAppend = [virtual(Element, '<org.w3c.dom.Element: org.w3c.dom.Node appendChild(org.w3c.dom.Node)>', [Element, Child])].



%% JDOM non terminals

jdom:newDocument(Doc, NewDocument) 
-->
	find(new(Doc, 'org.jdom.Document')),
	find(special(_, '<org.jdom.Document: void <init>()>', [Doc])),
	{NewDocument = [new(Doc, 'org.jdom.Document'), special(_, '<org.jdom.Document: void <init>()>', [Doc])]}.


jdom:newElement(Elt, NewElement) 
-->
	find(new(Elt, 'org.jdom.Element')),
	find(special(_, '<org.jdom.Element: void <init>(java.lang.String)>',[Elt, String])),
	{NewElement = [new(Elt, 'org.jdom.Element'), special(_, '<org.jdom.Element: void <init>(java.lang.String)>',[Elt, String])]}.


jdom:addContent(Element, Child, AddContent) 
-->
	find(virtual(Element, '<org.jdom.Element: org.jdom.Element addContent(org.jdom.Content)>', [Element, Child])),
	{AddContent = [virtual(Element, '<org.jdom.Element: org.jdom.Element addContent(org.jdom.Content)>', [Element, Child])]}.


jdom:setText(Element, SetText) 
-->
	find(virtual(Element, '<org.jdom.Element: org.jdom.Element setText(java.lang.String)>', [Element, _])),
	{SetText = [virtual(Element, '<org.jdom.Element: org.jdom.Element setText(java.lang.String)>', [Element, _])]}.

jdom:addContentToDoc(Doc, Content, AddContent) 
-->
	find(virtual(Doc, '<org.jdom.Document: org.jdom.Document addContent(org.jdom.Content)>', [Doc,Content])),
	{AddContent = [virtual(Doc, '<org.jdom.Document: org.jdom.Document addContent(org.jdom.Content)>', [Doc,Content])]}.


find(Event, [Event|Tail], Tail). %:- write('Finding: '), write(Event), nl.


print_list([]).
print_list([Event|Tail]) :- write('\t'), write(Event), nl, print_list(Tail).


print_jdom_dom_list([]).
print_jdom_dom_list([(JDom, Dom)|Tail]) :- 
	write('===> '), 
	nl,
	write('\tJDOM:'), print_list(JDom),
	nl,
	write('\tDOM:'), print_list(Dom),
	nl, 
	print_jdom_dom_list(Tail).



