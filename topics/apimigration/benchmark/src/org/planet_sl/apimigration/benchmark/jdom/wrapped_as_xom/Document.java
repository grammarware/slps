package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Wrapping;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;
import org.planet_sl.apimigration.benchmark.anno.Issue;
import static org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.Utils.content2node;
import static org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.Utils.node2content;

import java.util.List;

import org.jdom.output.XMLOutputter;
import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@SuppressWarnings("unchecked")
@MapsTo("org.jdom.Document")
public class Document extends ParentNode {
	
	@Wrapping
	org.jdom.Document document;

	@Wrapping
	Document(org.jdom.Document document) {
		this.document = document;
	}

	// XOM api starts below

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.Document#clone()")
	public Document(Document doc) {
		this((org.jdom.Document) doc.document.clone());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.Document(org.jdom.Element)")
	public Document(Element root) {
		this(new org.jdom.Document(root.element));
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Document#clone()")
	public Node copy() {
		return new Document((org.jdom.Document) document.clone());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Document#getBaseURI()")
	public String getBaseURI() {
		if (document.getBaseURI() == null) {
			return "";
		}
		return document.getBaseURI();
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.Document#getDocType()")
	public DocType getDocType() {
		return new DocType(document.getDocType());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.Document#getRootElement()")
	public Element getRootElement() {
		return new Element(document.getRootElement());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "")
	@Override
	@MapsTo("")
	public String getValue() {
		return document.getRootElement().getValue();
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre(value = "XOM prevents root element to be removed", resolved = true)
	@Override
	@MapsTo("org.jdom.Document#removeContent(int)")
	public Node removeChild(int position) {
		if (document.getContent(position) == document.getRootElement()) {
			throw new WellformednessException("removed root");
		}
		return content2node(document.removeContent(position));
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre(value = "XOM disallows root removal; throws if child is not a child of this document", resolved = true)
	@Override
	@MapsTo("org.jdom.Document#removeChild(Content)")
	public Node removeChild(Node child) {
		if (node2content(child) == document.getRootElement()) {
			throw new WellformednessException("removed root");
		}
		int index = document.indexOf(node2content(child));
		if (index == -1) {
			throw new NoSuchChildException("no such child");
		}
		document.removeContent(node2content(child));
		return child;
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "")
	@Issue.Pre(value = "xom requires that newchild is an element if oldchild is", resolved = true)
	@Issue.Throws(value = "xom has a special exception for multiple parent situation", resolved = true)
	@Override
	@MapsTo("")
	public void replaceChild(Node oldChild, Node newChild) {
		/*
		 * MultipleParentException - if newChild already has a parent
		 * NoSuchChildException - if oldChild is not a child of this node
		 * NullPointerException - if either argument is null IllegalAddException
		 * - if newChild is an attribute or a text node WellformednessException
		 * - if newChild oldChild is an element and newChild is not
		 */
		if (oldChild instanceof Element && !(newChild instanceof Element)) {
			throw new WellformednessException("invalid newChild");
		}
		if (newChild instanceof Attribute || newChild instanceof Text) {
			throw new IllegalAddException("invalid newChild");
		}
		org.jdom.Content content = node2content(newChild);
		if (content.getParent() != null) {
			throw new MultipleParentException("newchild already has parent");
		}
		document.setContent(document.indexOf(node2content(oldChild)),
				node2content(newChild));
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Document#setBaseURI(String)")
	public void setBaseURI(String uri) {
		document.setBaseURI(uri);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre(value = "xom allows a doctype to be already attached if it is attached to this document but does not allow null", resolved = true)
	@Issue.Throws(value = "special exception for multiple parent issue", resolved = true)
	@MapsTo("org.jdom.Document#setDocType(org.jdom.DocType)")
	public void setDocType(DocType doctype) {
		if (doctype == null) {
			throw new NullPointerException("null doctype");
		}
		if (doctype.doctype.getDocument() == document) {
			return;
		}
		if (doctype.doctype.getDocument() != null) {
			throw new MultipleParentException("multiple parents for doctype");
		}
		document.setDocType(((DocType) doctype).doctype);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre(value = "xom allows a root to be already attached if it is attached to this document but does not allow null", resolved = true)
	@Issue.Throws(value = "special exception for multiple parent issue", resolved = true)
	@MapsTo("org.jdom.Document#setRootElement(org.jdom.Element)")
	public void setRootElement(Element root) {
		if (root == null) {
			throw new NullPointerException("null root element");
		}
		if (root.element.getParent() == document) {
			return;
		}
		if (root.element.getParent() != null) {
			throw new MultipleParentException("root elt has parent");
		}
		document.setRootElement(((Element) root).element);
	}

	@Progress(value = Status.DONTCARE, comment = "is a debugging aid")
	@Solution(value = Strategy.EXTERNAL_MACRO, comment = "")
	@Issue.Post("the resulting String may be slightly different")
	@Override
	@MapsTo("")
	public String toXML() {
		return new XMLOutputter().outputString(document);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre(value = "XOM disallows null and Text nodes", resolved = true)
	@Issue.Throws("special exception for multiple parent situation")
	@Override
	@MapsTo("org.jdom.Document#addContent(org.jdom.Content)")
	public void appendChild(Node child) {
		if (child == null) {
			throw new NullPointerException("child is null");
		}
		if (child instanceof Text) {
			throw new IllegalAddException("text not allowed at root");
		}
		if (child.getParent() != null) {
			throw new MultipleParentException("child already has parent");
		}
		try {
			document.addContent(node2content(child));
		} catch (org.jdom.IllegalAddException e) {
			throw new IllegalAddException("illegal add", e);
		}
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Document#getContent(int)")
	public Node getChild(int position) {
		return content2node(document.getContent(position));
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Document#getContentSize()")
	public int getChildCount() {
		return document.getContentSize();
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Document#indexOf(org.jdom.Content)")
	public int indexOf(Node child) {
		return document.indexOf(node2content(child));
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Document#addContent(int,org.jdom.Content)")
	public void insertChild(Node child, int position) {
		try {
			document.addContent(position, node2content(child));
		} catch (org.jdom.IllegalAddException e) {
			throw new IllegalAddException("illegal add on document", e);
		}
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "detach is absent on jdom documents")
	@MapsTo("")
	public void detach() {
		// nop: documents have no parent.
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Document#getDocument()")
	public Document getDocument() {
		return new Document(document.getDocument());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Document#getParent()")
	public ParentNode getParent() {
		org.jdom.Parent parent = document.getParent();
		if (parent instanceof org.jdom.Element) {
			return new Element((org.jdom.Element) parent);
		}
		if (parent instanceof org.jdom.Document) {
			return new Document((org.jdom.Document) parent);
		}
		throw new AssertionError("Invalid parent for this document");
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.EXTERNAL_MACRO, comment = "")
	@Issue.Post("unclear how XPathContext affects the result")
	@Override
	@MapsTo("")
	public Nodes query(String query, XPathContext namespaces) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath
					.newInstance(query);
			for (Object o : namespaces.namespaces) {
				xpath.addNamespace((org.jdom.Namespace) o);
			}
			List list = xpath.selectNodes(this.document);
			return new Nodes(list);
		} catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.EXTERNAL_MACRO, comment = "")
	@Issue.Post("unclear how XPathContext affects the result")
	@Override
	@MapsTo("")
	public Nodes query(String query) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath
					.newInstance(query);
			List list = xpath.selectNodes(this.document);
			return new Nodes(list);
		} catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.Document#equals(Object)")
	public boolean equals(Object o) {
		if (!(o instanceof Document)) {
			return false;
		}
		return document.equals(((Document) o).document);
	}

	@Progress(value = Status.DONTCARE, comment = "opaque")
	@Override
	@MapsTo("org.jdom.Document#hashCode()")
	public int hashCode() {
		return document.hashCode();
	}

}