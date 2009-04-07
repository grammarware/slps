package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

public abstract class Node {

	public abstract Node copy();

	public abstract Node getChild(int position);

	public abstract int getChildCount();

	public abstract String getValue();

	public abstract String toXML();

	public abstract void detach();

	public abstract Document getDocument();

	public abstract ParentNode getParent();

	public abstract Nodes query(String query, XPathContext namespaces);

	public abstract Nodes query(String query);

	public abstract String getBaseURI();
}
