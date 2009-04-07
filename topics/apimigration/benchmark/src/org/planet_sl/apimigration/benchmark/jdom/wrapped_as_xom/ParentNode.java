package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

public abstract class ParentNode extends Node {

	public abstract void appendChild(Node child);

	public abstract int indexOf(Node child);

	public abstract Node removeChild(int position);

	public abstract Node removeChild(Node child);

	public abstract void replaceChild(Node oldChild, Node newChild);

	public abstract void setBaseURI(String uri);

	public abstract void insertChild(Node child, int position);
}
