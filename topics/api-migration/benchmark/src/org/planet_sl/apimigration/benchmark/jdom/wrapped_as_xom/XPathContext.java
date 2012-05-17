package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;
import org.planet_sl.apimigration.benchmark.anno.Issue;
import java.util.ArrayList;
import java.util.List;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@MapsTo("")
public class XPathContext {
	List<org.jdom.Namespace> namespaces;
	
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
	public  XPathContext()  {
		namespaces = new ArrayList<org.jdom.Namespace>();
		namespaces.add(org.jdom.Namespace.XML_NAMESPACE);
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
	public  XPathContext(String prefix, String uri)  {
		this();
		addNamespace(prefix, uri);
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
	public  void addNamespace(String prefix, String uri)  {
		namespaces.add(org.jdom.Namespace.getNamespace(prefix, uri));	
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
	@SuppressWarnings("unchecked")
	public static  XPathContext  makeNamespaceContent(Element element)  {
		XPathContext ctx = new XPathContext();
		ctx.namespaces.addAll(((Element)element).element.getAdditionalNamespaces());
		ctx.namespaces.add(((Element)element).element.getNamespace());
		return ctx;
	}
}