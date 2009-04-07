package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;


import static org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.Utils.content2node;
import static org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.Utils.node2content;


@SuppressWarnings("unchecked")
@MapsTo("org.jdom.Element")
public class Element extends ParentNode {
	org.jdom.Element element;
	
	
	Element(org.jdom.Element element) {
		this.element = element;
	}

	/// XOM api starts below
	
	@MapsTo("org.jdom.Element#clone()")
	public Element(Element element) {
		this((org.jdom.Element)element.element.clone());
	}

	@MapsTo("org.jdom.Element(String)")
	public Element(String name) {
		// TODO: XOM probably allows prefixed names here
		// but JDOM's interface does not allow us to create prefix elements without URIs
		if (name == null) {
			throw new NullPointerException("null name");
		}
		try {
			element = new org.jdom.Element(name);
		}
		catch (org.jdom.IllegalNameException e) {
			throw new IllegalNameException(e,name);
		}
	}

	@MapsTo("org.jdom.Element(String,String,String)")
	public Element(String name, String uri) {
		int index = name.indexOf(":");
		String prefix = "";
		String localName = name;
		if (index != -1) {
			prefix = name.substring(0, index);
			localName = name.substring(index + 1, name.length());
		}
		try {
			element = new org.jdom.Element(localName, prefix, uri);
		}
		catch (org.jdom.IllegalNameException e) {
			throw new IllegalNameException(e, name);
		}
	}
	
	@Override
	@MapsTo("org.jdom.Element#addContent(org.jdom.Content)")
	public void appendChild(Node child) {
		try {
			element.addContent(node2content(child));
		}
		catch (org.jdom.IllegalAddException e) {
			throw new IllegalAddException(e.getMessage(), e);
		}
	}

	@Override
	@MapsTo("org.jdom.Element#getContent(int)")
	public Node getChild(int position) {
		return content2node(element.getContent(position));
	}

	@Override
	@MapsTo("org.jdom.getContentSize()")
	public int getChildCount() {
		return element.getContentSize();
	}

	@Override
	@MapsTo("org.jdom.Element#indexOf(org.jdom.Content)")
	public int indexOf(Node child) {
		return element.indexOf(node2content(child));
	}

	@Override
	@MapsTo("org.jdom.Element(int,org.jdom.Content)")
	public void insertChild(Node child, int position) {
		if (child == null) {
			throw new NullPointerException("inserting null child");
		}
		element.addContent(position, node2content(child));
	}

	@Override
	@MapsTo("org.jdom.Element#removeContent(int)")
	public Node removeChild(int position) {
		return content2node(element.removeContent(position));
	}

	@Override
	@MapsTo("org.jdom.Element@removeContent(org.jdom.Content)")
	public Node removeChild(Node child) {
		boolean removed = element.removeContent(node2content(child));
		if (!removed) {
			throw new NoSuchChildException("not a child of this element: " + child);
		}
		return child;
	}

	@Override
	@MapsTo("")
	public void replaceChild(Node oldChild, Node newChild) {
		if (oldChild == null) {
			throw new NullPointerException("oldChild == null");
		}
		if (newChild == null) {
			throw new NullPointerException("newChild == null");
		}
		
		if (newChild instanceof Document) {
			throw new IllegalAddException("cannot add documents to elements");
		}
		
		if (newChild instanceof DocType) {
			throw new IllegalAddException("cannot add doctypes to elements");
		}
		
		if (newChild instanceof Attribute) {
			throw new IllegalAddException("cannot add attributes to elements");
		}
		
		int index = element.indexOf(node2content(oldChild));
		if (index == -1) {
			throw new NoSuchChildException("oldChild is not a child of this element");
		}
		try {
			element.setContent(index, node2content(newChild));
		}
		catch (org.jdom.IllegalAddException e) {
			throw new MultipleParentException(e);
		}

	}

	@Override
	@MapsTo("")
	public void setBaseURI(String uri) {
		/*
		 * Sets the URI against which relative URIs in this node will be
		 * resolved. Generally, it's only necessary to set this property if it's
		 * different from a node's parent's base URI, as it may be in a document
		 * assembled from multiple entities or by XInclude.
		 * 
		 * Relative URIs are not allowed here. Base URIs must be absolute.
		 * However, the base URI may be set to null or the empty string to
		 * indicate that the node has no explicit base URI. In this case, it
		 * inherits the base URI of its parent node, if any.
		 * 
		 * URIs with fragment identifiers are also not allowed. The value passed
		 * to this method must be a pure URI, not a URI reference.
		 * 
		 * You can also add an xml:base attribute to an element in the same way
		 * you'd add any other namespaced attribute to an element. If an
		 * element's base URI conflicts with its xml:base attribute, then the
		 * value found in the xml:base attribute is used.
		 * 
		 * If the base URI is null or the empty string and there is no xml:base
		 * attribute, then the base URI is determined by the nearest ancestor
		 * node which does have a base URI. Moving such a node from one location
		 * to another can change its base URI.
		 * 
		 * Parameters: URI - the new base URI for this node Throws:
		 * MalformedURIException - if URI is not a legal RFC 3986 absolute URI
		 */
		
		if (uri == null || uri.equals("")) {
			element.removeAttribute("base", org.jdom.Namespace.XML_NAMESPACE);
			return;
		}
		try {
			URI uriObject = new URI(uri);
			if (uriObject.getFragment() != null) {
				throw new MalformedURIException("no fragments allows in base URIs");
			}
			if (!uriObject.isAbsolute()) {
				throw new MalformedURIException("base URIs should be absolute.");
			}
			try {
				element.setAttribute("base", uriObject.toASCIIString(), org.jdom.Namespace.XML_NAMESPACE);
			}
			catch (org.jdom.IllegalDataException e) {
				throw new MalformedURIException("illegal data in uri");
			}
		}
		catch (URISyntaxException e) {
			throw new MalformedURIException(e, uri);
		}
		
	}

	@Override
	@MapsTo("org.jdom.Element#clone()")
	public Node copy() {
		return new Element((org.jdom.Element)element.clone());
	}

	@Override
	@MapsTo("org.jdom.Element#detach()")
	public void detach() {
		if (element.getDocument().getRootElement() == element) {
			throw new WellformednessException("detached root");
		}
		element.detach();
	}

	@Override
	@MapsTo("")
	public String getBaseURI() {
		/* 
		 * The base URI of an element is determined as follows:
		 * 
		 * If the element has an xml:base attribute, then the value of that
		 * attribute is converted from an IRI to a URI, absolutized if possible,
		 * and returned. Otherwise, if any ancestor element of the element
		 * loaded from the same entity has an xml:base attribute, then the value
		 * of that attribute from the nearest such ancestor is converted from an
		 * IRI to a URI, absolutized if possible, and returned. xml:base
		 * attributes from other entities are not considered. Otherwise, if
		 * setBaseURI() has been invoked on this element, then the URI most
		 * recently passed to that method is absolutized if possible and
		 * returned. Otherwise, if the element comes from an externally parsed
		 * entity or the document entity, and the original base URI has not been
		 * changed by invoking setBaseURI(), then the URI of that entity is
		 * returned. Otherwise, (the element was created by a constructor rather
		 * then being parsed from an existing document), the base URI of the
		 * nearest ancestor that does have a base URI is returned. If no
		 * ancestors have a base URI, then the empty string is returned.
		 * Absolutization takes place as specified by the XML Base
		 * specification. However, it is not always possible to absolutize a
		 * relative URI, in which case the empty string will be returned.
		 * 
		 * Returns: the base URI of this node
		 */
		// TODO: this probably incorrect in some cases.
		org.jdom.Attribute base = element.getAttribute("base", org.jdom.Namespace.XML_NAMESPACE);
		if (base != null) {
			if (base.getValue().equals("")) {
				String uri = element.getDocument().getBaseURI();
				return uri == null ? "" : uri;
			}
			return base.getValue();
		}
		if (getParent() != null) {
			return getParent().getBaseURI();
		}
		return "";
	}

	@Override
	@MapsTo("org.jdom.Element#getDocument()")
	public Document getDocument() {
		if (element.getDocument() == null) {
			return null;
		}
		return new Document(element.getDocument());
	}

	@Override
	@MapsTo("org.jdom.Element#getParent()")
	public ParentNode getParent() {
		org.jdom.Parent parent = element.getParent();
		if (parent == null) {
			return (ParentNode)null;
		}
		if (parent instanceof org.jdom.Element) {
			return new Element((org.jdom.Element)parent);
		}
		if (parent instanceof org.jdom.Document) {
			return new Document((org.jdom.Document)parent);
		}
		throw new AssertionError("invalid parent for element");
	}

	@Override
	@MapsTo("org.jdom.Element#getValue()")
	public String getValue() {
		return element.getValue();
	}

	@Override
	@MapsTo("")
	public Nodes query(String query, XPathContext namespaces) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			for (Object o: namespaces.namespaces) {
				xpath.addNamespace((org.jdom.Namespace)o);
			}
			List list = xpath.selectNodes(this.element);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}
	}

	@Override
	@MapsTo("")
	public Nodes query(String query) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			List list = xpath.selectNodes(this.element);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}	
	}

	@Override
	@MapsTo("")
	public String toXML() {
		return new org.jdom.output.XMLOutputter().outputString(element);
	}

	@MapsTo("org.jdom.Element#setAttribute()")
	public void addAttribute(Attribute attribute) {
		try {
			element.setAttribute(((Attribute)attribute).attribute);
		}
		catch (org.jdom.IllegalAddException e) {
			throw new MultipleParentException(e);
		}
	}

	@MapsTo("org.jdom.Element#addNamespaceDeclaration(org.jdom.Namespace)")
	public void addNamespaceDeclaration(String prefix, String uri) {
		try {
			element.addNamespaceDeclaration(org.jdom.Namespace.getNamespace(prefix, uri));
		}
		catch (org.jdom.IllegalAddException e) {
			throw new NamespaceConflictException(e);
		}
	}

	@MapsTo("org.jdom.Element#addContent(String)")
	public void appendChild(String text) {
		element.addContent(text);
	}

	@MapsTo("org.jdom.Element#getAttribute(String)")
	// TODO: does not use the mapped-to method.
	public Attribute getAttribute(String name) {
		org.jdom.Attribute attr = element.getAttribute(name, org.jdom.Namespace.NO_NAMESPACE);
		if (attr == null) {
			return null;
		}
		return new Attribute(attr);
	}

	@MapsTo("org.jdom.Element#getAttribute(String,org.jdom.Namespace)")
	public Attribute getAttribute(String localName, String namespaceURI) {
		org.jdom.Attribute attr;
		try {
			attr = element.getAttribute(localName, org.jdom.Namespace.getNamespace(namespaceURI));
		}
		catch (org.jdom.IllegalNameException e) {
			return null; // so here XOM returns null???
		}
		if (attr == null) {
			return null;
		}
		return new Attribute(attr);
	}

	@MapsTo("")
	public Attribute getAttribute(int index) {
		return new Attribute((org.jdom.Attribute)element.getAttributes().get(index));
	}

	@MapsTo("")
	public int getAttributeCount() {
		return element.getAttributes().size();
	}

	@MapsTo("org.jdom.Element#getAttributeValue(String)")
	// TODO: does not use the mapped-to method.
	public String getAttributeValue(String name) {
		org.jdom.Attribute attr = element.getAttribute(name, org.jdom.Namespace.NO_NAMESPACE);
		if (attr == null) {
			return null;
//			throw new NoSuchAttributeException("element has no attribute with " + name);
		}
		return attr.getValue();
	}

	@MapsTo("org.jdom.Element#getAttributeValue(String,org.jdom.Namespace")
	public String getAttributeValue(String localName, String namespaceURI) {
		org.jdom.Attribute attr;
		try {
			attr = element.getAttribute(localName, org.jdom.Namespace.getNamespace(namespaceURI));
		}
		catch (org.jdom.IllegalNameException e) {
			return null;
		}
		if (attr == null) {
			return null;
		}
		return attr.getValue();
	}

	@MapsTo("org.jdom.Element#getChildren(String)")
	public Elements getChildElements(String name) {
		List list = element.getChildren(name);
		return new Elements(list);
	}

	@MapsTo("org.jdom.Element#getChildren(String,org.jdom.Namespace)")
	public Elements getChildElements(String localName, String namespaceURI) {
		if (localName != null && localName.equals("")) {
			localName = null;
		}
		return new Elements(element.getChildren(localName, org.jdom.Namespace.getNamespace(namespaceURI)));
	}

	@MapsTo("org.jdom.Element#getChildren()")
	public Elements getChildElements() {
		return new Elements(element.getChildren());
	}

	@MapsTo("")
	public Element getFirstChildElement(String name) {
		List list = element.getChildren(name);
		if (list.size() > 0) {
			return new Element((org.jdom.Element) list.get(0));
		}
		return null;
	}

	@MapsTo("")
	public Element getFirstChildElement(String localName, String namespaceURI) {
		List list = element.getChildren(localName, org.jdom.Namespace.getNamespace(namespaceURI));
		if (list.size() > 0) {
			return new Element((org.jdom.Element) list.get(0));
		}
		return null;
	}

	@MapsTo("org.jdom.Element#getName()")
	public String getLocalName() {
		return element.getName();
	}

	@MapsTo("")
	public int getNamespaceDeclarationCount() {
		return element.getAdditionalNamespaces().size();
	}

	@MapsTo("org.jdom.Element#getNamespacePrefix")
	public String getNamespacePrefix() {
		return element.getNamespacePrefix();
	}

	@MapsTo("")
	public String getNamespacePrefix(int index) {
		// TODO: this is probably an API mismatch
		// A leaky abstraction.
		// XOM includes the namespace of the element itself (where JDOM does not for getAdditionalNamespaces)
		// yet the order XOM uses is arbitrary.
		return ((org.jdom.Namespace)element.getAdditionalNamespaces().get(index)).getPrefix();
	}

	@MapsTo("org.jdom.Element#getNamespaceURI()")
	public String getNamespaceURI() {
		return element.getNamespaceURI();
	}

	@MapsTo("")
	public String getNamespaceURI(String prefix) {
		return element.getNamespace(prefix).getURI();
	}

	@MapsTo("org.jdom.Element#getQualifiedName()")
	public String getQualifiedName() {
		return element.getQualifiedName();
	}

	@MapsTo("")
	public void insertChild(String text, int position) {
		if (text == null) {
			throw new NullPointerException("inserting null strinng");
		}
		element.addContent(position, new org.jdom.Text(text));
	}

	@MapsTo("org.jdom.Element#removeAttribute(org.jdom.Attribute)")
	public Attribute removeAttribute(Attribute attribute) {
		boolean removed = element.removeAttribute(((Attribute)attribute).attribute);
		if (!removed) {
			throw new NoSuchAttributeException("no such attribute: " + attribute);
		}
		return attribute;
	}

	@MapsTo("org.jdom.Element#removeContent()")
	public Nodes removeChildren() {
		return new Nodes(element.removeContent());
	}

	@MapsTo("org.jdom.Element#removeNamespaceDeclaration(org.jdom.Namespace)")
	public void removeNamespaceDeclaration(String prefix) {
		org.jdom.Namespace ns = element.getNamespace(prefix);
		element.removeNamespaceDeclaration(ns);
	}

	@MapsTo("org.jdom.Element#setName(String)")
	public void setLocalName(String localName) {
		try {
			element.setName(localName);
		}
		catch (org.jdom.IllegalNameException e) {
			throw new IllegalNameException(e, localName);
		}
	}

	@MapsTo("")
	public void setNamespacePrefix(String prefix) {
		// TODO: probably wrong; unsupported feature?
		try {
			element.setNamespace(org.jdom.Namespace.getNamespace(prefix, element.getNamespaceURI()));
		}
		catch (org.jdom.IllegalNameException e) {
			throw new IllegalNameException(e, prefix);
		}
	}

	@MapsTo("")
	public void setNamespaceURI(String uri) {
//		if (uri.equals("")) {
//			throw new NamespaceConflictException("unsetting namespace");
//		}
		element.setNamespace(org.jdom.Namespace.getNamespace(element.getNamespacePrefix(), uri));
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof Element)) {
			return false;
		}
		return element.equals(((Element)o).element);
	}
	
	@Override
	public int hashCode() {
		return element.hashCode();
	}
}
