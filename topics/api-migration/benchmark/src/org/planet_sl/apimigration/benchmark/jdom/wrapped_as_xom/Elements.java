package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.List;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;
import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Wrapping;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;

@SuppressWarnings("unchecked")
@MapsTo("java.util.List")
public class Elements {

	@Wrapping
	List list;

	@Wrapping
	Elements(List list) {
		this.list = list;
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("java.util.List#get(int)")
	public Element get(int index) {
		return new Element((org.jdom.Element) list.get(index));
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("java.util.List#size()")
	public int size() {
		return list.size();
	}

}