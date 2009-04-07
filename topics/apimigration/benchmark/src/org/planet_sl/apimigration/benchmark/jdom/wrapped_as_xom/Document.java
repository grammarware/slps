package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import static org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.Utils.content2node;
import static org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.Utils.node2content;


import java.util.List;

import org.jdom.output.XMLOutputter;

@SuppressWarnings("unchecked")
public class Document extends ParentNode {
	org.jdom.Document document;
	
	Document(org.jdom.Document document) {
		this.document = document;
	}
	
	// XOM api starts below
	
	public Document(Document doc) {
		this((org.jdom.Document)doc.document.clone());
	}
	
	public Document(Element root) {
		this(new org.jdom.Document(root.element));
	}

	@Override
	public Node copy() {
		return new Document((org.jdom.Document)document.clone());
	}

	@Override
	public String getBaseURI() {
		return document.getBaseURI();
	}

	public DocType getDocType() {
		return new DocType(document.getDocType());
	}

	public Element getRootElement() {
		return new Element(document.getRootElement());
	}

	@Override
	public String getValue() {
		return document.getRootElement().getValue();
	}

	@Override
	public Node removeChild(int position) {
		return content2node(document.removeContent(position));
	}
	
	@Override
	public Node removeChild(Node child) {
		return content2node(document.removeContent(document.indexOf(node2content(child))));
	}

	@Override
	public void replaceChild(Node oldChild, Node newChild) {
		document.setContent(document.indexOf(node2content(oldChild)), node2content(newChild));
	}

	@Override
	public void setBaseURI(String uri) {
		document.setBaseURI(uri);
	}

	public void setDocType(DocType doctype) {
		document.setDocType(((DocType)doctype).doctype);

	}

	public void setRootElement(Element root) {
		document.setRootElement(((Element)root).element);
	}

	@Override
	public String toXML() {
		return new XMLOutputter().outputString(document);
	}

	@Override
	public void appendChild(Node child) {
		document.addContent(node2content(child));
	}

	@Override
	public Node getChild(int position) {
		return content2node(document.getContent(position));
	}

	@Override
	public int getChildCount() {
		return document.getContentSize();
	}

	@Override
	public int indexOf(Node child) {
		return document.indexOf(node2content(child));
	}

	@Override
	public void insertChild(Node child, int position) {
		document.addContent(position, node2content(child));
	}

	@Override
	public void detach() {
		// nop: documents have no parent.
	}

	@Override
	public Document getDocument() {
		return this;
	}

	@Override
	public ParentNode getParent() {
		return null;
	}

	@Override
	public Nodes query(String query, XPathContext namespaces) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			for (Object o: namespaces.namespaces) {
				xpath.addNamespace((org.jdom.Namespace)o);
			}
			List list = xpath.selectNodes(this.document);
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
			List list = xpath.selectNodes(this.document);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}	
	}
	
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof Document)) {
			return false;
		}
		return document.equals(((Document)o).document);
	}
	
	@Override
	public int hashCode() {
		return document.hashCode();
	}

}
