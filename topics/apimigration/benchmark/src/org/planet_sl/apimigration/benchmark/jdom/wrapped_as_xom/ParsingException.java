package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@SuppressWarnings("serial")
@MapsTo("org.jdom.JDOMParseException")
public class ParsingException extends Exception {

	private String uri;

//	public ParsingException(String message, Throwable cause) {
//		super(message, cause);
//	}

	public ParsingException(String message, String uri) {
		super(message);
		this.uri = uri;
	}

	public ParsingException(String message, Throwable cause, String uri) {
		super(message, cause);
		this.uri = uri;
	}

	public ParsingException(String message, Throwable cause) {
		super(message, cause);
	}

	public String getURI() {
		return uri;
	}

}
