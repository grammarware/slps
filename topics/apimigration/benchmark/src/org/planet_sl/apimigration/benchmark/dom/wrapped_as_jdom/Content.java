package org.planet_sl.apimigration.benchmark.dom.wrapped_as_jdom;

public class Content {

	protected String text = null;
	protected org.w3c.dom.Node domNode = null;

	/*
	 * JDOM Api (Excerpt)
	 */

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

	/*
	 * Utils
	 */
	
	public String toString() {
		return text;
	}

	/*package*/ Content() {
	}
	
	/*package*/ Content(String text) {
		this.text = text;
	}
	
	/*package*/ void build(Element parent) {
		domNode = parent.domNode.getOwnerDocument().createTextNode(text);
		((org.w3c.dom.Element)parent.domNode).appendChild(domNode);
	}

}
