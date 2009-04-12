package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;
import org.planet_sl.apimigration.benchmark.anno.Issue;
import org.planet_sl.apimigration.benchmark.anno.MapsTo;

public abstract class ParentNode extends Node {

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@MapsTo("")
	public abstract void appendChild(Node child);

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@MapsTo("org.jdom.Parent#indexOf(Content)")
	public abstract int indexOf(Node child);

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@MapsTo("org.jdom.Parent#removeContent(int)")
	public abstract Node removeChild(int position);

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@MapsTo("org.jdom.Parent#removeContent(Content)")
	public abstract Node removeChild(Node child);

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@MapsTo("")
	public abstract void replaceChild(Node oldChild, Node newChild);

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@MapsTo("")
	public abstract void setBaseURI(String uri);

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	@MapsTo("")
	public abstract void insertChild(Node child, int position);

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "")
	@MapsTo("")
	public Node getChild(int position) {
		throw new IndexOutOfBoundsException("comments have no children");
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "")
	@MapsTo("")
	public int getChildCount() {
		return 0;
	}
}