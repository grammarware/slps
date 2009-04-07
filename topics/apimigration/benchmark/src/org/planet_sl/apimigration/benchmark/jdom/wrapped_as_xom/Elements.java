package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.List;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@SuppressWarnings("unchecked")
@MapsTo("java.util.List")
public class Elements {

	List list;
	
	@MapsTo("")
	public Elements(List list) {
		this.list = list;
	}
	
	
	@MapsTo("java.util.List#get(int)")
	public Element get(int index) {
		return new Element((org.jdom.Element)list.get(index));
	}

	@MapsTo("java.util.List#size()")
	public int size() {
		return list.size();
	}

}
