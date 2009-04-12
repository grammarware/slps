package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;
import org.planet_sl.apimigration.benchmark.anno.Issue;
import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@MapsTo("")
public abstract class Node {

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public abstract Node copy();

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public abstract String getValue();

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public abstract String toXML();

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public abstract void detach();

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public abstract Document getDocument();

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public abstract ParentNode getParent();

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public abstract Nodes query(String query, XPathContext namespaces);

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public abstract Nodes query(String query);

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public abstract String getBaseURI();
	
	
	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public abstract int getChildCount();
	
	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public abstract Node getChild(int i);
}