package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;


import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;
import org.planet_sl.apimigration.benchmark.anno.Issue;public class Utils {
	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public static  Node  content2node(org.jdom.Content content)  {
		if (content instanceof org.jdom.Element) {
			return new Element((org.jdom.Element)content);
		}
		if (content instanceof org.jdom.Comment) {
			return new Comment((org.jdom.Comment)content);
		}
		if (content instanceof org.jdom.Text) {
			return new Text((org.jdom.Text)content);
		}
		if (content instanceof org.jdom.DocType) {
			return new DocType((org.jdom.DocType)content);
		}
		if (content instanceof org.jdom.CDATA) {
			return new Text((org.jdom.CDATA)content, true);
		}
		if (content instanceof org.jdom.ProcessingInstruction) {
			return new ProcessingInstruction((org.jdom.ProcessingInstruction)content);
		}
		throw new AssertionError("Not all content cases covered");
	}
	
	@Progress(
		value = Status.TODO, 
		comment = ""
	)
	@Solution(
		value = Strategy.OTHER,
		comment = ""
	)
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public static  org.jdom.Content  node2content(Node child)  {
		if (child instanceof Element) {
			return ((Element)child).element;
		}
		if (child instanceof Text && ((Text)child).isCDATA) {
			return ((Text)child).cdata;
		}
		if (child instanceof Text) {
			return ((Text)child).text;
		}
		if (child instanceof ProcessingInstruction) {
			return ((ProcessingInstruction)child).pi;
		}
		if (child instanceof Comment) {
			return ((Comment)child).comment;
		}
		if (child instanceof DocType) {
			return ((DocType)child).doctype;
		}
		throw new IllegalArgumentException("Nodes of type " + child.getClass().getName()
				+ " are not Content in JDOM");
	}


}