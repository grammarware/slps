package org.planet_sl.apimigration.benchmark.dom.wrapped_as_jdom;

import java.util.LinkedList;
import java.util.List;

public class Element extends Content {

	String name = null;
	private List<Content> kids = new LinkedList<Content>();
	
	/*
	 * JDOM API (excerpt)
	 */

	public Element(String name) {
		this.name = name;
	}

	
	public String getName() {
		if (domNode != null) {
			return ((org.w3c.dom.Element)domNode).getTagName();
		}
		return name;
	}
	
	// NB: if an element is added multiple times it means it'll get "built" multiple times
	// Ralf: I disagree because you are checking for domNode != null.
	public Element addContent(Content elt) {
		kids.add(elt);
		if (domNode != null) {
			elt.build(this);
		}
		return this;
	}

	public Content getChild(String name) {
		if (domNode == null) {
			for (Content kid: kids) {
				if (kid instanceof Element) {
					Element elt = (Element)kid;
					if (name.equals(elt.getName())) {
						return elt;
					}
				}
			}
		}
		else {
			org.w3c.dom.NodeList nodeList = ((org.w3c.dom.Element)domNode).getChildNodes();
			for (int i = 0; i < nodeList.getLength(); i++) {
				org.w3c.dom.Node node = nodeList.item(i);
				if (node instanceof org.w3c.dom.Element) {
					if (((org.w3c.dom.Element)node).getTagName().equals(name)) {
						return new Element(node);
					}	
				}
			}
		}
		return null;
	}

	@Override
	public void setText(String text) {
		if (domNode != null) {
			//domNode.setTextContent(text);
			domNode.appendChild(domNode.getOwnerDocument().createTextNode(text));
		}
		else {
			this.text = text;
			//kids.add(new Content(text));
		}
	}

	@Override
	public String getText() {
		if (domNode != null) {
			//return ((org.w3c.dom.Element)domNode).getChildNodes().item(0).getNodeValue();
			return domNode.getTextContent();
		}
		//return kids.get(0).getText();
		return text;
	}
	
	/*
	 * Utilities
	 */

	/* For wrapping. */
	/*package*/ Element(org.w3c.dom.Node node) {
		this.domNode = node; 
	}

	/*package*/ void build(org.w3c.dom.Document document) {
		domNode = document.getDocumentElement();
		for (Content kid: kids) {
			kid.build(this);
		}
	}
	
	/*package*/ void build(Element parent) {
		domNode = parent.domNode.getOwnerDocument().createElement(name);
		((org.w3c.dom.Element)parent.domNode).appendChild(domNode);
		if (text != null) {
			domNode.setTextContent(text);
		}
		for (Content kid: kids) {
			kid.build(this);
		}
	}
	
	
	/* For simple serialization. */
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("<" + getName());
		if (kids.size() == 0 && text == null) {
			builder.append("/>");
		}
		else {
			builder.append(">");
			for (Content kid: kids) {
				builder.append(kid);
			}
			if (text != null) {
				builder.append(text);
			}
			builder.append("</" + name + ">");
		}
		return builder.toString();
	}

}
