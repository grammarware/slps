package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@MapsTo("")
public class Namespace extends Node {

	public static final String XML_NAMESPACE = org.jdom.Namespace.XML_NAMESPACE.getURI();
	org.jdom.Namespace namespace;
	org.jdom.Element parent;
	
	Namespace(org.jdom.Namespace namespace) {
		this(namespace, null);
	}

	Namespace(org.jdom.Namespace namespace, org.jdom.Element parent) {
		this.namespace = namespace;
		this.parent = parent;
	}

	public Namespace(String prefix, String URI, org.jdom.Element parent) {
		this(org.jdom.Namespace.getNamespace(prefix, URI), parent);
		parent.addNamespaceDeclaration(namespace);
	}
	

	
	public Node copy() {
		return new Namespace((org.jdom.Namespace)namespace, parent);
	}

	public void detach() {
		parent = null;
	}

	
	public String getPrefix() {
		return namespace.getPrefix();
	}

	public String getValue() {
		return namespace.getURI();
	}

	public String toXML() {
		return "xmlns:" + getPrefix() + "=\"" + getValue() + "\"";
	}


	public Document getDocument() {
		if (parent == null) {
			return null;
		}
		return new Document(parent.getDocument());
	}

	public ParentNode getParent() {
		if (parent == null) {
			return (ParentNode)null;
		}
		return new Element((org.jdom.Element)parent);
	}

	public Nodes query(String xpath, XPathContext namespaces) {
		return null;
	}

	public Nodes query(String xpath) {
		return null;
	}

	@Override
	public String getBaseURI() {
		return parent.getDocument().getBaseURI();
	}

}
