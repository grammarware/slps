package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.ArrayList;
import java.util.List;

public class XPathContext {
	List<org.jdom.Namespace> namespaces;
	
	public XPathContext() {
		namespaces = new ArrayList<org.jdom.Namespace>();
		namespaces.add(org.jdom.Namespace.XML_NAMESPACE);
	}
	
	public XPathContext(String prefix, String uri) {
		this();
		addNamespace(prefix, uri);
	}
	
	public void addNamespace(String prefix, String uri) {
		namespaces.add(org.jdom.Namespace.getNamespace(prefix, uri));	
	}
	
	@SuppressWarnings("unchecked")
	public static XPathContext makeNamespaceContent(Element element) {
		XPathContext ctx = new XPathContext();
		ctx.namespaces.addAll(((Element)element).element.getAdditionalNamespaces());
		ctx.namespaces.add(((Element)element).element.getNamespace());
		return ctx;
	}
}
