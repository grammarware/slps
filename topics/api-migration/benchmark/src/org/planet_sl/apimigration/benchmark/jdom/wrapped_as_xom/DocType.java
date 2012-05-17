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

import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@SuppressWarnings("unchecked")
@MapsTo("org.jdom.DocType")
public class DocType extends Node {

	@Wrapping
	org.jdom.DocType doctype;

	@Wrapping
	DocType(org.jdom.DocType doctype) {
		this.doctype = doctype;
	}

	// XOM api starts below

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.DocType#clone()")
	public DocType(DocType doctype) {
		this((org.jdom.DocType) doctype.doctype.clone());
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre("XOM does a lot more input validation on rootElementName")
	@Issue.Throws("for some invalid names (starting with digits) jdom *does* throw")
	@Unresolved(XML.DocTypeValidity)
	@MapsTo("org.jdom.DocType(String)")
	public DocType(String rootElementName) {
		if (rootElementName == null) {
			throw new IllegalNameException("null", rootElementName);
		}
		if (rootElementName.startsWith(":")) {
			throw new IllegalNameException("colon", rootElementName);
		}
		try {
			doctype = new org.jdom.DocType(rootElementName);
		}
		catch (org.jdom.IllegalNameException e) {
			throw new IllegalNameException(e, rootElementName);
		}
		catch (org.jdom.IllegalDataException e) {
			throw new WellformednessException("illegal data");
		}
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre("XOM does a lot more input validation on rootElementName etc.")
	@MapsTo("org.jdom.DocType(String,String)")
	@Unresolved(XML.DocTypeValidity)
	public DocType(String rootElementName, String systemID) {
		if (rootElementName == null) {
			throw new IllegalNameException("null", rootElementName);
		}
		if (rootElementName.startsWith(":")) {
			throw new IllegalNameException("colon", rootElementName);
		}
		String error = org.jdom.Verifier.checkSystemLiteral(systemID);
		if (error != null) {
			throw new IllegalDataException(error);
		}
		try {
			doctype = new org.jdom.DocType(rootElementName, systemID);
		}
		catch (org.jdom.IllegalNameException e) {
			throw new IllegalNameException(e, rootElementName);
		}
		catch (org.jdom.IllegalDataException e) {
			throw new WellformednessException("illegal data");
		}
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre("XOM does a lot more input validation on rootElementName etc.")
	@MapsTo("org.jdom.DocType(String,String,String)")
	@Unresolved(XML.DocTypeValidity)
	public DocType(String rootElementName, String publicID, String systemID) {
		if (rootElementName == null) {
			throw new IllegalNameException("null", rootElementName);
		}
		if (rootElementName.startsWith(":")) {
			throw new IllegalNameException("colon", rootElementName);
		}
		
		String error = org.jdom.Verifier.checkPublicID(publicID);
		if (error != null) {
			throw new WellformednessException(error);
		}
		error = org.jdom.Verifier.checkSystemLiteral(systemID);
		if (error != null) {
			throw new WellformednessException(error);
		}
		
		/// illegal chars etc .... etc.
		try {
			doctype = new org.jdom.DocType(rootElementName, publicID, systemID);
		}
		catch (org.jdom.IllegalNameException e) {
			throw new IllegalNameException(e, rootElementName);
		}
		catch (org.jdom.IllegalDataException e) {
			throw new WellformednessException("illegal data");
		}
	}

	
	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.DocType#clone()")
	public Node copy() {
		return new DocType((org.jdom.DocType) doctype.clone());
	}

	@Progress(value = Status.OK, comment = "tests fail because of parser differences")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.DocType#getInternalSubset()")
	public String getInternalDTDSubset() {
		String str = doctype.getInternalSubset(); 
		return str == null ? "" : str;
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.DocType#getPublicID()")
	public String getPublicID() {
		return doctype.getPublicID();
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.DocType#getElementName()")
	public String getRootElementName() {
		return doctype.getElementName();
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.DocType#getSystemID()")
	public String getSystemID() {
		return doctype.getSystemID();
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.DocType#getValue()")
	public String getValue() {
		// NB: in both XOM and JDOM this returns the empty string.
		return doctype.getValue();
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Issue.Pre("XOM parses the InternalSubset string and throws accordingly")
	@MapsTo("org.jdom.DocType#setInternalSubset(String)")
	@Unresolved(XML.DocTypeValidity)
	public void setInternalDTDSubset(String subset) {
		if (subset == null) {
			subset = "";
		}
		doctype.setInternalSubset(subset);
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre("Xom checks for invalid characters")
	@MapsTo("org.jdom.DocType#setPublicID(String)")
	@Unresolved(XML.DocTypeValidity)
	public void setPublicID(String id) {
		if (doctype.getSystemID() == null || doctype.getSystemID().equals("")) {
			throw new WellformednessException("public id without system id");
		}
		doctype.setPublicID(id);
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre("XOM checks for more invalid chars in rootElementNames ")
	@MapsTo("org.jdom.DocType#setElementName(String)")
	@Unresolved(XML.DocTypeValidity)
	public void setRootElementName(String name) {
		if (name == null) {
			throw new IllegalNameException("null", name);
		}
		if (name.startsWith(":")) {
			throw new IllegalNameException("colon", name);
		}
		if (name.matches("^[0-9]")) {
			throw new IllegalNameException("digit", name);
		}
		doctype.setElementName(name);
	}

	@Progress(Status.NEEDSWORK)
	@Solution(Strategy.ADVANCED_DELEGATE)
	@Issue.Pre("XOM checks for invalid chars")
	@Issue.Invariant(value = "XOM requires that Public ID has been nulled before SystemID can be", resolved = true)
	@MapsTo("org.jdom.DocType#setSystemId(String)")
	@Unresolved(XML.DocTypeValidity)
	public void setSystemID(String id) {
		if (id == null || id.equals("")) {
			if (!(doctype.getPublicID() == null || doctype.getPublicID().equals(""))) {
				throw new WellformednessException("removing system id before public id");
			}
		}
		doctype.setSystemID(id);
	}

	@Progress(value = Status.DONTCARE, comment = "debugging aid")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Issue.Post("output may differ slightly")
	@Override
	@MapsTo("org.jdom.output.XMLOutputter#outputString(org.jdom.OutputType)")
	@Unresolved(XML.Serialization)
	public String toXML() {
		org.jdom.output.Format form = org.jdom.output.Format.getRawFormat();
		form = form.setOmitEncoding(true);
		form = form.setLineSeparator("\n");
		org.jdom.output.XMLOutputter out = new org.jdom.output.XMLOutputter(form);
		String str = out.outputString(doctype);
		return str;
		//return str.substring(0, str.length() - 1);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.DocType#detach()")
	public void detach() {
		doctype.detach();
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "")
	@Override
	@MapsTo("")
	public String getBaseURI() {
		org.jdom.Parent parent = doctype.getParent();
		if (parent == null) {
			return "";
		}
		if (parent instanceof org.jdom.Document) {
			return ((org.jdom.Document)parent).getBaseURI();
		}
		if (parent instanceof org.jdom.Element) {
			return new Element(((org.jdom.Element)parent)).getBaseURI();
		}
		throw new AssertionError("Invalid parent");
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.DocType#getDocument()")
	public Document getDocument() {
		return new Document(doctype.getDocument());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.DocType#getParent()")
	public ParentNode getParent() {
		org.jdom.Parent parent = doctype.getParent();
		if (parent == null) {
			return null;
		}
		if (parent instanceof org.jdom.Element) {
			// should not occur in this case...
			return new Element((org.jdom.Element) parent);
		}
		if (parent instanceof org.jdom.Document) {
			return new Document((org.jdom.Document) parent);
		}
		throw new AssertionError("invalid parent for doctype");
	}


	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.xpath.XPath#selectNodes(Object)")
	public Nodes query(String query, XPathContext namespaces) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath
					.newInstance(query);
			for (Object o : namespaces.namespaces) {
				xpath.addNamespace((org.jdom.Namespace) o);
			}
			List list = xpath.selectNodes(this.doctype);
			return new Nodes(list);
		} catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Override
	@MapsTo("org.jdom.xpath.XPath#selectNodes(Object)")
	public Nodes query(String query) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath
					.newInstance(query);
			List list = xpath.selectNodes(this.doctype);
			return new Nodes(list);
		} catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}
	}
	
	@Progress(value = Status.DONTCARE, comment = "")
	@Solution(value = Strategy.CLONE, comment = "")
	@Override
	@MapsTo("")
	public String toString() {
		return  "[nu.xom.DocType: " + doctype.getElementName() + "]";
	}
}