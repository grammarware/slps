package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_dom;

import org.jdom.Content;


public class Element extends Node {
	Element(org.jdom.Element element) {
		super(element);
	}

	public void setAttribute(String name, String value) {
		org.jdom.Element elt = (org.jdom.Element)content;
		elt.setAttribute(name, value);
	}
	
	public void appendChild(Node node) {
		org.jdom.Element elt = (org.jdom.Element)content;
		elt.addContent(node.content);
	}

	public Node getFirstChild() {
		return wrap((Content) ((org.jdom.Element)content).getChildren().get(0));
	}

	public Node getNextSibling() {
		boolean found = false;
		for (Object kid: ((org.jdom.Element)content).getParentElement().getChildren()) {
			if (found) {
				return wrap((Content)kid);
			}
			if (kid == content) {
				found = true;
			}
		}
		return null;
	}

	public String getTextContent() {
		return content.getValue();
	}

}
