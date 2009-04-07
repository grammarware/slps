package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.List;

import org.jdom.output.XMLOutputter;

@SuppressWarnings("unchecked")
public class Text extends Node{
	org.jdom.Text text;
	
	Text(org.jdom.Text text) {
		this.text = text;
	}
	
	// XOM api starts below
	
	public Text(String data) {
        this(new org.jdom.Text(data));
    }

	public Text(Text text) {
        this((org.jdom.Text)text.text.clone());
    }
	
	
	Text() {
		// for CDATA
	}

	public void setValue(String data) {
		text.setText(data);
	}

	@Override
	public Node copy() {
		return new Text((org.jdom.Text)text.clone());
	}

	@Override
	public Node getChild(int position) {
		throw new IndexOutOfBoundsException("text nodes do not have children");     
	}

	@Override
	public int getChildCount() {
		return 0;
	}

	@Override
	public String getValue() {
		return text.getText();
	}

	@Override
	public String toXML() {
		return new XMLOutputter().outputString(text);
	}

	@Override
	public void detach() {
		text.detach();
	}

	@Override
	public String getBaseURI() {
		throw new UnsupportedOperationException("base URI only on documents in JDOM");
	}

	@Override
	public Document getDocument() {
		return new Document(text.getDocument());
	}

	@Override
	public ParentNode getParent() {
		org.jdom.Parent parent = text.getParent();
		if (parent instanceof org.jdom.Document) {
			return new Document((org.jdom.Document)parent);
		}
		if (parent instanceof org.jdom.Element) {
			return new Element((org.jdom.Element)parent);
		}
		throw new AssertionError("invalid parent for this org.jdom.Text: " + text);
	}

	@Override
	public Nodes query(String query, XPathContext namespaces) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			for (Object o: namespaces.namespaces) {
				xpath.addNamespace((org.jdom.Namespace)o);
			}
			List list = xpath.selectNodes(this.text);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}
	}

	@Override
	public Nodes query(String query) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			List list = xpath.selectNodes(this.text);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}	
	}

}
