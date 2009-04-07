package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@MapsTo("")
public abstract class Node {

	public abstract Node copy();

	public abstract String getValue();

	public abstract String toXML();

	public abstract void detach();

	public abstract Document getDocument();

	public abstract ParentNode getParent();

	public abstract Nodes query(String query, XPathContext namespaces);

	public abstract Nodes query(String query);

	public abstract String getBaseURI();

	public Node getChild(int position) {
		throw new IndexOutOfBoundsException("comments have no children");
	}

	public int getChildCount() {
		return 0;
	}
}
