package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_dom;

import org.jdom.filter.ElementFilter;

public class Document {
	public org.jdom.Document doc;
	
	Document(String nsUri, String tagName, String docType) {
		doc = new org.jdom.Document();
		doc.addContent(new org.jdom.Element(tagName));
	}

	public Element getDocumentElement() {
		return new Element(doc.getRootElement());
	}
	
	public Element createElement(String name) {
		return new Element(new org.jdom.Element(name));
	}
	
	public Node createTextNode(String text) {
		return new Node(new org.jdom.Text(text));
	}

	public NodeList getElementsByTagName(String string) {
		return new NodeList(doc.getDescendants(new ElementFilter(string)));
	}
}
