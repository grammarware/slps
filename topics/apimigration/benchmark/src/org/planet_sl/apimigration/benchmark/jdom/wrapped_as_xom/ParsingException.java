package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;
import org.planet_sl.apimigration.benchmark.anno.Issue;
import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@SuppressWarnings("serial")
@MapsTo("org.jdom.JDOMParseException")
public class ParsingException extends Exception {

	private String uri;

//	public ParsingException(String message, Throwable cause) {
//		super(message, cause);
//	}

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
	public  ParsingException(String message, String uri)  {
		super(message);
		this.uri = uri;
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
	public  ParsingException(String message, Throwable cause, String uri)  {
		super(message, cause);
		this.uri = uri;
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
	public  ParsingException(String message, Throwable cause)  {
		super(message, cause);
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
	public  String  getURI()  {
		return uri;
	}

}