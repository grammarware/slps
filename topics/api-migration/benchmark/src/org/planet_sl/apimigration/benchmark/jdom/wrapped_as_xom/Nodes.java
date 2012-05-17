package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;


import java.util.ArrayList;
import java.util.List;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;
import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Wrapping;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;

@SuppressWarnings("unchecked")
@MapsTo("java.util.List")
public class Nodes {

	@Wrapping
	List list;

	@Wrapping
	Nodes(List list) {
		this.list = list;
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "")
	@MapsTo("java.util.Collections.emptyList()")
	public Nodes() {
		this(new ArrayList<Object>());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "")
	@MapsTo("java.util.Collections.singletonList(Object)")
	public Nodes(Element elt) {
		if (elt == null) {
			throw new NullPointerException("null arg");
		}
		this.list = new ArrayList<Object>();
		this.list.add(elt);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("java.util.List#add(Object)")
	public void append(Node node) {
		if (node == null) {
			throw new NullPointerException("null append");
		}
		list.add(node);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("java.util.List#contains(Object)")
	public boolean contains(Node node) {
		return list.contains(node);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("java.util.List#get(int)")
	public Node get(int index) {
		return (Node)list.get(index);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("java.util.List#add(int,Object)")
	public void insert(Node node, int index) {
		if (node == null) {
			throw new NullPointerException("null insert");
		}
		list.add(index, node);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("java.util.List#remove(int)")
	public Node remove(int index) {
		return (Node) list.remove(index);
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("java.util.List#size()")
	public int size() {
		return list.size();
	}

}