package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.List;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@SuppressWarnings("unchecked")
@MapsTo("org.jdom.ProcessingInstruction")
public class ProcessingInstruction extends Node {
	org.jdom.ProcessingInstruction pi;

	ProcessingInstruction(org.jdom.ProcessingInstruction pi) {
		this.pi = pi;
	}
	
	// XOM API starts below
	
	@MapsTo("org.jdom.ProcessingInstruction#clone()")
	public ProcessingInstruction(ProcessingInstruction instruction) {
		this((org.jdom.ProcessingInstruction)instruction.pi.clone());
	}
	
	@MapsTo("org.jdom.ProcessingInstruction(String,String)")
	public ProcessingInstruction(String target, String data) {
		this(new org.jdom.ProcessingInstruction(target, data));
	}

	@Override
	@MapsTo("org.jdom.ProcessingInstruction#clone()")
	public Node copy() {
		return new ProcessingInstruction((org.jdom.ProcessingInstruction)pi.clone());
	}

	@Override
	@MapsTo("org.jdom.ProcessingInstruction#detach()")
	public void detach() {
		pi.detach();
	}

	@Override
	@MapsTo("")
	public String getBaseURI() {
		// TODO: Unsure if this is correct 
		return pi.getDocument().getBaseURI();
	}


	@Override
	@MapsTo("org.jdom.ProcessingInstruction#getDocument()")
	public Document getDocument() {
		return new Document(pi.getDocument());
	}

	@Override
	@MapsTo("org.jdom.ProcessingInstruction#getParent()")
	public ParentNode getParent() {
		org.jdom.Parent parent = pi.getParent();
		if (parent == null) {
			return null;
		}
		if (parent instanceof org.jdom.Element) {
			return new Element((org.jdom.Element)parent);
		}
		if (parent instanceof org.jdom.Document) {
			return new Document((org.jdom.Document)parent);
		}
		throw new AssertionError("invalid parent for element");
	}

	@Override
	@MapsTo("org.jdom.ProcessingInstruction#getValue()")
	public String getValue() {
		return pi.getValue();
	}

	@Override
	@MapsTo("")
	public Nodes query(String query, XPathContext namespaces) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			for (Object o: namespaces.namespaces) {
				xpath.addNamespace((org.jdom.Namespace)o);
			}
			List list = xpath.selectNodes(this.pi);
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
			List list = xpath.selectNodes(this.pi);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}	
	}

	@Override
	@MapsTo("")
	public String toXML() {
		return new org.jdom.output.XMLOutputter().outputString(pi);
	}

	@MapsTo("org.jdom.ProcessingInstruction#getTarget()")
	public String getTarget() {
		return pi.getTarget();
	}

	@MapsTo("org.jdom.ProcessingInstruction#setTarget(String)")
	public void setTarget(String target) {
		pi.setTarget(target);
	}

	@MapsTo("org.jdom.ProcessingInstruction#setData(String)")
	public void setValue(String data) {
		// TODO: not sure if this is correct.
		pi.setData(data);
	}
	
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof ProcessingInstruction)) {
			return false;
		}
		return pi.equals(((ProcessingInstruction)o).pi);
	}
	
	@Override
	public int hashCode() {
		return pi.hashCode();
	}

}
