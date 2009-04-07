package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;
import org.planet_sl.apimigration.benchmark.anno.Issue;
import java.util.List;

import org.jdom.CDATA;
import org.jdom.output.XMLOutputter;
import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@SuppressWarnings("unchecked")
@MapsTo("org.jdom.Text;org.jdom.CDATA")
public class Text extends Node{
	org.jdom.Text text;
	CDATA cdata;
	boolean isCDATA = false;
	
	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")  Text(org.jdom.Text text)  {
		this.text = text;
	}
	
	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")  Text(CDATA cdata, boolean isCDATA)  {
		this.cdata = cdata;
		this.isCDATA = isCDATA;
	}
	
	// XOM api starts below
	
	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@MapsTo("org.jdom.Text(String)")
	public  Text(String data)  {
        this(new org.jdom.Text(data));
    }

	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@MapsTo("org.jdom.Text#clone()")
	public  Text(Text text)  {
        this((org.jdom.Text)text.text.clone());
    }
	
	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@MapsTo("org.jdom.Text#setText(String)")
	public  void setValue(String data)  {
		text.setText(data);
	}

	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@Override
	@MapsTo("org.jdom.Text#clone();org.jdom.CDATA#clone()")
	public  Node  copy()  {
		if (isCDATA) {
			return new Text((org.jdom.CDATA)cdata.clone(), true);
		}
		return new Text((org.jdom.Text)text.clone());
	}

	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@Override
	@MapsTo("org.jdom.Text#getText();org.jdom.CDATA#getValue()")
	public  String  getValue()  {
		return isCDATA ? cdata.getValue() : text.getText();
	}

	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@Override
	@MapsTo("")
	public  String  toXML()  {
		return isCDATA ? new XMLOutputter().outputString(cdata) : new XMLOutputter().outputString(text);
	}

	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@Override
	@MapsTo("org.jdom.Text#detach();org.jdom.CDATA#detach()")
	public  void detach()  {
		if (isCDATA) {
			cdata.detach();
		}
		else {
			text.detach();
		}
	}

	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@Override
	@MapsTo("")
	public  String  getBaseURI()  {
		return getParent().getBaseURI(); // CHEATING
		//throw new UnsupportedOperationException("base URI only on documents in JDOM");
	}

	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@Override
	@MapsTo("org.jdom.Text#getDocument();org.jdom.CDATA#getDocument()")
	public  Document  getDocument()  {
		org.jdom.Document doc = isCDATA ? cdata.getDocument() : text.getDocument();
		if (doc == null) {
			return null;
		}
		return new Document(doc);
	}

	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@Override
	@MapsTo("org.jdom.Text#getParent();org.jdom.CDATA#getParent()")
	public  ParentNode  getParent()  {
		org.jdom.Parent parent = isCDATA ? cdata.getParent() : text.getParent();
		if (parent == null) {
			return (ParentNode)null;
		}
		if (parent instanceof org.jdom.Document) {
			return new Document((org.jdom.Document)parent);
		}
		if (parent instanceof org.jdom.Element) {
			return new Element((org.jdom.Element)parent);
		}
		throw new AssertionError("invalid parent for this org.jdom.Text: " + text);
	}

	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@Override
	@MapsTo("")
	public  Nodes  query(String query, XPathContext namespaces)  {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			for (Object o: namespaces.namespaces) {
				xpath.addNamespace((org.jdom.Namespace)o);
			}
			List list = xpath.selectNodes(isCDATA ? cdata : text);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}
	}

	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@Override
	@MapsTo("")
	public  Nodes  query(String query)  {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			List list = xpath.selectNodes(isCDATA ? cdata: text);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}	
	}

}