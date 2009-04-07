package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;


@SuppressWarnings("serial")
public class IllegalDataException extends RuntimeException {

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

	@Override
	public String getMessage() {
		return jdomException.getMessage();
	}

	public String getData() {
		return data;
	}
}
