package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.net.URISyntaxException;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;

@SuppressWarnings("serial")
@MapsTo("org.jdom.IllegalDataException")
public class MalformedURIException extends RuntimeException {

	@SuppressWarnings("unused")
	private URISyntaxException uriException;
	private String data;

	public MalformedURIException(String message) {
		super(message);
	}
	
	public MalformedURIException(URISyntaxException e) {
		this(e.getMessage());
		uriException = e;
	}

	public MalformedURIException(URISyntaxException e, String data) {
		this(e);
		this.data = data;
	}

	public String getData() {
		return data;
	}
}
