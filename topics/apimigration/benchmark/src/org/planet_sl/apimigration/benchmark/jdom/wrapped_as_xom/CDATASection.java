package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.List;

@SuppressWarnings("unchecked")
public class CDATASection extends Text {
	org.jdom.CDATA cdata;
	
	CDATASection(org.jdom.CDATA cdata) {
		super();
		this.cdata = cdata;
	}
	
	@Override
	public Node copy() {
		return new CDATASection((org.jdom.CDATA)cdata.clone());
	}

	@Override
	public void detach() {
		cdata.detach();
	}

	@Override
	public String getBaseURI() {
		// Here we have to little knowledge to go up the tree:
		// we need the Element parent of this node, not the org.jdom.Element
		// parent, so we cannot implement this correctly (?).
		return cdata.getDocument().getBaseURI();
	}

	@Override
	public Node getChild(int position) {
		throw new IndexOutOfBoundsException("cdata sections have children");
	}

	@Override
	public int getChildCount() {
		return 0;
	}

	@Override
	public Document getDocument() {
		return new Document(cdata.getDocument());
	}

	@Override
	public ParentNode getParent() {
		org.jdom.Parent parent = cdata.getParent();
		// TODO: move this to utils; it's the same in attribute etc.
		if (parent instanceof org.jdom.Element) {
			return new Element((org.jdom.Element)parent);
		}
		if (parent instanceof org.jdom.Document) {
			return new Document((org.jdom.Document)parent);
		}
		throw new AssertionError("invalid parent for attribute");
	}

	@Override
	public String getValue() {
		return cdata.getValue();
	}

	@Override
	public Nodes query(String query, XPathContext namespaces) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			for (Object o: namespaces.namespaces) {
				xpath.addNamespace((org.jdom.Namespace)o);
			}
			List list = xpath.selectNodes(this.cdata);
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
			List list = xpath.selectNodes(this.cdata);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}	
	}

	@Override
	public String toXML() {
		return "<![CDATA[" + cdata.getValue() + "]]>";
	}

	@Override
	public void setValue(String data) {
		cdata.setText(data);
	}

}
