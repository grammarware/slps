package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;



public class Namespace extends Node {

	org.jdom.Namespace namespace;
	
	Namespace(org.jdom.Namespace namespace) {
		this.namespace = namespace;
	}

	public Namespace(String prefix, String URI, Element parent) {
		this(org.jdom.Namespace.getNamespace(prefix, URI));
		parent.element.addNamespaceDeclaration(namespace);
	}
	

	public Node copy() {
		// TODO Auto-generated method stub
		return null;
	}

	public void detach() {
		// TODO Auto-generated method stub

	}

	public Node getChild(int position) {
		// TODO Auto-generated method stub
		return null;
	}

	public int getChildCount() {
		// TODO Auto-generated method stub
		return 0;
	}

	public String getPrefix() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getValue() {
		// TODO Auto-generated method stub
		return null;
	}

	public String toXML() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getBaseURI() {
		// TODO Auto-generated method stub
		return null;
	}

	public Document getDocument() {
		// TODO Auto-generated method stub
		return null;
	}

	public ParentNode getParent() {
		// TODO Auto-generated method stub
		return null;
	}

	public Nodes query(String xpath, XPathContext namespaces) {
		// TODO Auto-generated method stub
		return null;
	}

	public Nodes query(String xpath) {
		// TODO Auto-generated method stub
		return null;
	}

}
