package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@MapsTo("org.jdom.Parent")
public abstract class ParentNode extends Node {

	@MapsTo("")
	public abstract void appendChild(Node child);

	@MapsTo("org.jdom.Parent#indexOf(Content)")
	public abstract int indexOf(Node child);

	@MapsTo("org.jdom.Parent#removeContent(int)")
	public abstract Node removeChild(int position);

	@MapsTo("org.jdom.Parent#removeContent(Content)")
	public abstract Node removeChild(Node child);

	@MapsTo("")
	public abstract void replaceChild(Node oldChild, Node newChild);

	@MapsTo("")
	public abstract void setBaseURI(String uri);

	@MapsTo("")
	public abstract void insertChild(Node child, int position);
}
