package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_dom;

public class Node {
	protected org.jdom.Content content;
	
	Node(org.jdom.Content node) {
		this.content = node;
	}
	
	protected static Node wrap(org.jdom.Content node) {
		if (node instanceof org.jdom.Element)
			return new Element((org.jdom.Element)node);
		else
			return new Node(node);
	}
}
