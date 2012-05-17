package org.planet_sl.apimigration.benchmark.dom.wrapped_as_jdom;

// Ralf: Where is this one used?
public class Namespace {

	private String nsURI;

	private Namespace(String nsURI) {
		this.nsURI = nsURI;
	}
	
	public static Namespace getNamespace(String nsURI) {
		return new Namespace(nsURI);
	}

	public String getNsURI() {
		return nsURI;
	}

	
}
