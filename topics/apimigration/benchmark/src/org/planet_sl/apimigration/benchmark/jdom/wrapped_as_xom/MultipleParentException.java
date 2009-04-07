package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;


@SuppressWarnings("serial")
public class MultipleParentException extends RuntimeException {

	private org.jdom.IllegalAddException jdomException;

	public MultipleParentException(org.jdom.IllegalAddException e) {
		this.jdomException = e;
	}

	@Override
	public String getMessage() {
		return jdomException.getMessage();
	}
	
}
