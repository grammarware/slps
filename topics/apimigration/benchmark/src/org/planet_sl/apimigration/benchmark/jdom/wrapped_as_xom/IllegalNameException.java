package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;


@SuppressWarnings("serial")
public class IllegalNameException extends RuntimeException {

	private org.jdom.IllegalNameException jdomException;
	private String data;

	public IllegalNameException(org.jdom.IllegalNameException e, String localName) {
		super(e.getMessage());
		this.jdomException = e;
		this.data = localName;
	}
	
	public IllegalNameException(String string, String data) {
		super(string);
		this.data = data;
	}

	public String getData() {
		return data;
	}

}
