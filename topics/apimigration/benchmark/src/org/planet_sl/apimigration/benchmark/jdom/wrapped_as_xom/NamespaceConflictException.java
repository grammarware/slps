package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;
import org.planet_sl.apimigration.benchmark.anno.Issue;
import org.jdom.IllegalAddException;
import org.jdom.IllegalNameException;
import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@SuppressWarnings("serial")
@MapsTo("org.jdom.IllegalAddException") // TODO: check this
public class NamespaceConflictException extends RuntimeException {

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
	public  NamespaceConflictException(IllegalNameException e, String uri)  {
		super(e.getMessage());
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
	public  NamespaceConflictException(String string)  {
		super(string);
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
	public  NamespaceConflictException(IllegalAddException e)  {
		super(e.getMessage());
	}

	public NamespaceConflictException(IllegalAddException e, String defaultNs,
			String uri) {

		super(" default namespace " + uri + " conflicts with " + defaultNs);
	}

}