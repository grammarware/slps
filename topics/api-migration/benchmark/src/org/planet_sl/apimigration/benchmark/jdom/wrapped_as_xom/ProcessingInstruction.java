package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.List;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;
import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Wrapping;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;

@SuppressWarnings("unchecked")
@MapsTo("org.jdom.ProcessingInstruction")
public class ProcessingInstruction extends Node {
	
	@Wrapping
	org.jdom.ProcessingInstruction pi;

	@Wrapping
	ProcessingInstruction(org.jdom.ProcessingInstruction pi) {
		this.pi = pi;
	}

	// XOM API starts below

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.ProcessingInstruction#clone()")
	public ProcessingInstruction(ProcessingInstruction instruction) {
		this((org.jdom.ProcessingInstruction) instruction.pi.clone());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.ProcessingInstruction(String,String)")
	public ProcessingInstruction(String target, String data) {
		String error = org.jdom.Verifier.checkProcessingInstructionTarget(target);
		if (error != null) {
			throw new IllegalTargetException(error, target);
		}
		error = org.jdom.Verifier.checkProcessingInstructionData(data);
		if (error != null) {
			throw new IllegalDataException(error, data);
		}
		if (data.indexOf("\r") > -1) {
			throw new IllegalDataException("allowed cr", data);
		}
		if (data != null && !data.equals("")) {
			if (Character.isWhitespace(data.charAt(0))) {
				throw new IllegalDataException("allowed leading space", data);
			}
		}
		pi = new org.jdom.ProcessingInstruction(target, data);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.ProcessingInstruction#clone()")
	public Node copy() {
		return new ProcessingInstruction((org.jdom.ProcessingInstruction) pi
				.clone());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.ProcessingInstruction#detach()")
	public void detach() {
		pi.detach();
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("")
	public String getBaseURI() {
		org.jdom.Parent parent = pi.getParent();
		if (parent instanceof org.jdom.Document) {
			return pi.getDocument().getBaseURI();
		}
		if (parent instanceof org.jdom.Element) {
			return new Element((org.jdom.Element)parent).getBaseURI();
		}
		throw new AssertionError("invalid parent for doctype");
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.ProcessingInstruction#getDocument()")
	public Document getDocument() {
		return new Document(pi.getDocument());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.ProcessingInstruction#getParent()")
	public ParentNode getParent() {
		org.jdom.Parent parent = pi.getParent();
		if (parent == null) {
			return null;
		}
		if (parent instanceof org.jdom.Element) {
			return new Element((org.jdom.Element) parent);
		}
		if (parent instanceof org.jdom.Document) {
			return new Document((org.jdom.Document) parent);
		}
		throw new AssertionError("invalid parent for element");
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.ProcessingInstruction#getValue()")
	public String getValue() {
		return pi.getValue();
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "")
	@Override
	@MapsTo("org.jdom.xpath.XPath#selectNodes(Object)")
	public Nodes query(String query, XPathContext namespaces) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath
					.newInstance(query);
			for (Object o : namespaces.namespaces) {
				xpath.addNamespace((org.jdom.Namespace) o);
			}
			List list = xpath.selectNodes(this.pi);
			return new Nodes(list);
		} catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "")
	@Override
	@MapsTo("org.jdom.xpath.XPath#selectNodes(Object)")
	public Nodes query(String query) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath
					.newInstance(query);
			List list = xpath.selectNodes(this.pi);
			return new Nodes(list);
		} catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}
	}

	@Progress(value = Status.DONTCARE, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.output.XMLOutputter#outputString(org.jdom.ProcessingInstruction)")
	public String toXML() {
		return new org.jdom.output.XMLOutputter().outputString(pi);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.ProcessingInstruction#getTarget()")
	public String getTarget() {
		return pi.getTarget();
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.ProcessingInstruction#setTarget(String)")
	public void setTarget(String target) {
		String error = org.jdom.Verifier.checkProcessingInstructionTarget(target);
		if (error != null) {
			throw new IllegalTargetException(error, target);
		}
		pi.setTarget(target);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.ProcessingInstruction#setData(String)")
	public void setValue(String data) {
		String error = org.jdom.Verifier.checkProcessingInstructionData(data);
		if (error != null) {
			throw new IllegalDataException(error, data);
		}
		pi.setData(data);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof ProcessingInstruction)) {
			return false;
		}
		return pi.equals(((ProcessingInstruction) o).pi);
	}

	@Progress(value = Status.DONTCARE, comment = "opaque")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	public int hashCode() {
		return pi.hashCode();
	}

	@Progress(value = Status.DONTCARE, comment = "debugging")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	public String toString() {
		String str = pi.getData().replaceAll("\n", "\\\\n");
		if (str.length() > 50) {
			str = str.substring(0, 50) + "...";
		}
		return "[nu.xom.ProcessingInstruction: target=\"" + pi.getTarget() +
			"\"; data=\"" + str + "\"]";
	}
	

}