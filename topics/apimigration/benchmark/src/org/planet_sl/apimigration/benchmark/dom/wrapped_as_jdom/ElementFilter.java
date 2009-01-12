package org.planet_sl.apimigration.benchmark.dom.wrapped_as_jdom;

// Ralf: used at all? It would be needed though for getDescendants and getContent!
public class ElementFilter {

	private String name;

	public ElementFilter(String name) {
		this.setName(name);
	}

	private void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	
}
