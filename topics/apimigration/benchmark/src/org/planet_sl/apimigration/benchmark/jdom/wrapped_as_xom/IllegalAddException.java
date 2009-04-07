package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

@SuppressWarnings("serial")
public class IllegalAddException extends RuntimeException {

	org.jdom.IllegalAddException jdomException;

	public IllegalAddException(String message, org.jdom.IllegalAddException e) {
		this(message);
		this.jdomException = e;
	}
	
	public IllegalAddException(String string) {
		super(string);
	}

	
}
