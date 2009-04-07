package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.List;


import static org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.Utils.content2node;
import static org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.Utils.node2content;

@SuppressWarnings("unchecked")
public class Nodes {

	List list;
	
	Nodes(List list) {
		this.list = list;
	}
	
	public void append(Node node) {
		list.add(node2content(node));
	}

	public boolean contains(Node node) {
		return list.contains(node2content(node));
	}

	public Node get(int index) {
		return content2node((org.jdom.Content)list.get(index));
	}

	public void insert(Node node, int index) {
		list.add(index, node2content(node));
	}

	public Node remove(int index) {
		return (Node)content2node((org.jdom.Content)list.remove(index));
	}

	public int size() {
		return list.size();
	}

}
