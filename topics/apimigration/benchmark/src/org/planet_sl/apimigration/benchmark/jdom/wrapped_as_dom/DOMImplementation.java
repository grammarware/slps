package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_dom;

public class DOMImplementation {

	public Document createDocument(String nsUri, String tagName, String docType) {
		return new Document(nsUri, tagName, docType);
	}
}
