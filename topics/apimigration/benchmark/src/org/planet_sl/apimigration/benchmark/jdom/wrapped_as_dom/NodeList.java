package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_dom;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class NodeList {

	private List<Node> list;
	
	@SuppressWarnings("unchecked")
	public NodeList(Iterator iter) {
		list = new LinkedList<Node>();
		while (iter.hasNext()) {
			list.add(Node.wrap((org.jdom.Content)iter.next()));
		}
	}

	public int getLength() {
		return list.size();
	}

	public Node item(int i) {
		return list.get(i);
	}

}
