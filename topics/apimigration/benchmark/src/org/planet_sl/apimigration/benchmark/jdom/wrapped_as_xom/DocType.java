package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.List;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@SuppressWarnings("unchecked")
@MapsTo("org.jdom.DocType")
public class DocType extends Node {

	org.jdom.DocType doctype;

	DocType(org.jdom.DocType doctype) {
		this.doctype = doctype;
	}
	
	// XOM api starts below
	
	@MapsTo("org.jdom.DocType#clone()")
	public DocType(DocType doctype) {
		this((org.jdom.DocType)doctype.doctype.clone());
	}
	
	@MapsTo("org.jdom.DocType(String)")
	public DocType(String rootElementName) {
		if (rootElementName == null) {
			throw new IllegalNameException("null", rootElementName);
		}
		if (rootElementName.startsWith(":")) {
			throw new IllegalNameException("colon", rootElementName);
		}
		doctype = new org.jdom.DocType(rootElementName);
	}

	@MapsTo("org.jdom.DocType(String,String)")
	public DocType(String rootElementName, String systemID) {
		this(new org.jdom.DocType(rootElementName, systemID));
	}

	@MapsTo("org.jdom.DocType(String,String,String)")
	public DocType(String rootElementName, String publicID, String systemID) {
		this(new org.jdom.DocType(rootElementName, publicID, systemID));
	}
	
	@Override
	@MapsTo("org.jdom.DocType#clone()")
	public Node copy() {
		return new DocType((org.jdom.DocType)doctype.clone());
	}

	@MapsTo("org.jdom.DocType#getInternalSubset()")
	public String getInternalDTDSubset() {
		return doctype.getInternalSubset();
	}

	@MapsTo("org.jdom.DocType#getPublicID()")
	public String getPublicID() {
		return doctype.getPublicID();
	}

	@MapsTo("org.jdom.DocType#getElementName()")
	public String getRootElementName() {
		return doctype.getElementName();
	}

	@MapsTo("org.jdom.DocType#getSystemID()")
	public String getSystemID() {
		return doctype.getSystemID();
	}

	@Override
	@MapsTo("org.jdom.DocType#getValue()")
	public String getValue() {
		// NB: in both XOM and JDOM this returns the empty string.
		return doctype.getValue();
	}

	@MapsTo("org.jdom.DocType#setInternalSubset(String)")
	public void setInternalDTDSubset(String subset) {
		doctype.setInternalSubset(subset);
	}

	@MapsTo("org.jdom.DocType#setPublicID(String)")
	public void setPublicID(String id) {
		doctype.setPublicID(id);
	}

	@MapsTo("org.jdom.DocType#setElementName(String)")
	public void setRootElementName(String name) {
		doctype.setElementName(name);
	}

	@MapsTo("org.jdom.DocType#setSystemId(String)")
	public void setSystemID(String id) {
		doctype.setSystemID(id);
	}

	@Override
	@MapsTo("")
	public String toXML() {
		return new org.jdom.output.XMLOutputter().outputString(doctype);
	}

	@Override
	@MapsTo("org.jdom.DocType#detach()")
	public void detach() {
		doctype.detach();
	}

	@Override
	@MapsTo("")
	public String getBaseURI() {
		return doctype.getDocument().getBaseURI();
	}

	@Override
	@MapsTo("org.jdom.DocType#getDocument()")
	public Document getDocument() {
		return new Document(doctype.getDocument());
	}

	@Override
	@MapsTo("org.jdom.DocType#getParent()")
	public ParentNode getParent() {
		org.jdom.Parent parent = doctype.getParent();
		if (parent == null) {
			return null;
		}
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
	@MapsTo("")
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
	@MapsTo("")
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
