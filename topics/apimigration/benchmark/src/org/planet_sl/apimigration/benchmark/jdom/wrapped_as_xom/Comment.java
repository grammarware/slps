package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Unresolved;
import org.planet_sl.apimigration.benchmark.anno.Wrapping;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;
import org.planet_sl.apimigration.benchmark.anno.Unresolved.XML;
import org.planet_sl.apimigration.benchmark.anno.Issue;
import java.util.List;

import org.jdom.output.XMLOutputter;
import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@SuppressWarnings("unchecked")
@MapsTo("org.jdom.Comment")
public class Comment extends Node {
	@Wrapping
	org.jdom.Comment comment;

	@Wrapping
	Comment(org.jdom.Comment comment) {
		this.comment = comment;
	}

	// / XOM api starts below

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.Comment#clone()")
	public Comment(Comment comment) {
		this((org.jdom.Comment) comment.comment.clone());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre(value = "XOM is more picky about invalid chars in comment data", resolved = true)
	@MapsTo("org.jdom.Comment(String)")
	public Comment(String data) {
		if (data.indexOf("\r") > -1) {
			// NOTE: more specific exception.
			throw new IllegalCharacterDataException("invalid char in comment",
					data);
		}
		try {
			comment = new org.jdom.Comment(data);
		} catch (org.jdom.IllegalDataException e) {
			throw new IllegalCharacterDataException(e, data);
		}
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Comment#clone()")
	public Node copy() {
		return new Comment((org.jdom.Comment) comment.clone());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Comment#detach()")
	public void detach() {
		comment.detach();
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "")
	@Override
	@MapsTo("")
	public String getBaseURI() {
		return getParent().getBaseURI();
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Comment#getDocument()")
	public Document getDocument() {
		if (comment.getDocument() == null) {
			return null;
		}
		return new Document(comment.getDocument());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Comment#getParent()")
	public ParentNode getParent() {
		org.jdom.Parent parent = comment.getParent();
		if (parent == null) {
			return (ParentNode) parent;
		}
		if (parent instanceof org.jdom.Element) {
			return new Element((org.jdom.Element) parent);
		}
		if (parent instanceof org.jdom.Document) {
			return new Document((org.jdom.Document) parent);
		}
		throw new AssertionError("invalid parent for comment "
				+ parent.getClass());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Comment#getValue()")
	public String getValue() {
		return comment.getValue();
	}

	@Progress(value = Status.OK, comment = "")
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
			List list = xpath.selectNodes(this.comment);
			return new Nodes(list);
		} catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "")
	@Override
	@MapsTo("org.jdom.xpath.XPath#selectNodes(Object)")
	public Nodes query(String query) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath
					.newInstance(query);
			List list = xpath.selectNodes(this.comment);
			return new Nodes(list);
		} catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}
	}

	@Progress(value = Status.DONTCARE, comment = "is debugging aid")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Issue.Post("the resulting string may be slightly different")
	@Override
	@Unresolved(XML.Serialization)
	@MapsTo("org.jdom.output.XMLOutputter#outputString(org.jdom.Comment)")
	public String toXML() {
		return new XMLOutputter().outputString(comment);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre(value = "data may be null in XOM", resolved = true)
	@MapsTo("org.jdom.Comment#setText(String)")
	public void setValue(String data) {
		if (data == null) {
			data = "";
		}
		try {
			comment.setText(data);
		} catch (org.jdom.IllegalDataException e) {
			// We throw a more specific exception here
			// which is subclass of illegaldataexception
			throw new IllegalCharacterDataException(e, data);
		}
	}


}