package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.List;


@SuppressWarnings("unchecked")
public class Comment extends Node {
	org.jdom.Comment comment;
	
	Comment(org.jdom.Comment comment) {
		this.comment = comment;
	}

	/// XOM api starts below
	
	public Comment(Comment comment) {
		this((org.jdom.Comment)comment.comment.clone());
	}

	public Comment(String data) {
		this(new org.jdom.Comment(data));
	}
	
	@Override
	public Node copy() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void detach() {
		// TODO Auto-generated method stub

	}

	@Override
	public String getBaseURI() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Node getChild(int position) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getChildCount() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public Document getDocument() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ParentNode getParent() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getValue() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Nodes query(String query, XPathContext namespaces) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			for (Object o: namespaces.namespaces) {
				xpath.addNamespace((org.jdom.Namespace)o);
			}
			List list = xpath.selectNodes(this.comment);
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
			List list = xpath.selectNodes(this.comment);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}	
	}

	@Override
	public String toXML() {
		// TODO Auto-generated method stub
		return null;
	}

	public void setValue(String data) {
		// TODO Auto-generated method stub
		
	}

}
