package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.List;

public class Elements {

	@SuppressWarnings("unchecked")
	List list;
	
	@SuppressWarnings("unchecked")
	public Elements(List list) {
		this.list = list;
	}
	
	public Element get(int index) {
		return new Element((org.jdom.Element)list.get(index));
	}

	public int size() {
		return list.size();
	}

}
