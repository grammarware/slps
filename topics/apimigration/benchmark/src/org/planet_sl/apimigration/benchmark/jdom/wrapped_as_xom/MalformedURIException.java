package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.net.URISyntaxException;

@SuppressWarnings("serial")
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
