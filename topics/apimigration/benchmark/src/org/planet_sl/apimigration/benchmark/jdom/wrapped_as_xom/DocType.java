package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.List;

@SuppressWarnings("unchecked")
public class DocType extends Node {

	org.jdom.DocType doctype;

	DocType(org.jdom.DocType doctype) {
		this.doctype = doctype;
	}
	
	// XOM api starts below
	
	public DocType(DocType doctype) {
		this((org.jdom.DocType)doctype.doctype.clone());
	}
	
	public DocType(String rootElementName) {
		this(new org.jdom.DocType(rootElementName));
	}

	public DocType(String rootElementName, String systemID) {
		this(new org.jdom.DocType(rootElementName, systemID));
	}

	public DocType(String rootElementName, String publicID, String systemID) {
		this(new org.jdom.DocType(rootElementName, publicID, systemID));
	}
	
	@Override
	public Node copy() {
		return new DocType((org.jdom.DocType)doctype.clone());
	}

	@Override
	public Node getChild(int position) {
		throw new IndexOutOfBoundsException("doctypes don't have children");
	}

	@Override
	public int getChildCount() {
		return 0;
	}

	public String getInternalDTDSubset() {
		return doctype.getInternalSubset();
	}

	public String getPublicID() {
		return doctype.getPublicID();
	}

	public String getRootElementName() {
		return doctype.getElementName();
	}

	public String getSystemID() {
		return doctype.getSystemID();
	}

	@Override
	public String getValue() {
		// NB: in both XOM and JDOM this returns the empty string.
		return doctype.getValue();
	}

	public void setInternalDTDSubset(String subset) {
		doctype.setInternalSubset(subset);
	}

	public void setPublicID(String id) {
		doctype.setPublicID(id);
	}

	public void setRootElementName(String name) {
		doctype.setElementName(name);
	}

	public void setSystemID(String id) {
		doctype.setSystemID(id);
	}

	@Override
	public String toXML() {
		return new org.jdom.output.XMLOutputter().outputString(doctype);
	}

	@Override
	public void detach() {
		doctype.detach();
	}

	@Override
	public String getBaseURI() {
		return doctype.getDocument().getBaseURI();
	}

	@Override
	public Document getDocument() {
		return new Document(doctype.getDocument());
	}

	@Override
	public ParentNode getParent() {
		org.jdom.Parent parent = doctype.getParent();
		if (parent instanceof org.jdom.Element) {
			// should not occur in this case...
			return new Element((org.jdom.Element)parent);
		}
		if (parent instanceof org.jdom.Document) {
			return new Document((org.jdom.Document)parent);
		}
		throw new AssertionError("invalid parent for doctype");	
	}

	@Override
	public Nodes query(String query, XPathContext namespaces) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			for (Object o: namespaces.namespaces) {
				xpath.addNamespace((org.jdom.Namespace)o);
			}
			List list = xpath.selectNodes(this.doctype);
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
			List list = xpath.selectNodes(this.doctype);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}	
	}
}
