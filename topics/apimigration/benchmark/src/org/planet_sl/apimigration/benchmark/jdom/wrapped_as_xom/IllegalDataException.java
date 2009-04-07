package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;


@SuppressWarnings("serial")
@MapsTo("org.jdom.IllegalDataException")
public class IllegalDataException extends WellformednessException {

	private org.jdom.IllegalDataException jdomException;
	private String data;

	public IllegalDataException(org.jdom.IllegalDataException jdomException) {
		super(jdomException.getMessage());
		this.jdomException = jdomException;
	}
	
	public IllegalDataException(org.jdom.IllegalDataException e, String data) {
		this(e);
		this.data = data;
	}

	public IllegalDataException(String message, String data) {
		super(message);
		this.data = data;
	}

	public String getData() {
		return data;
	}
}
