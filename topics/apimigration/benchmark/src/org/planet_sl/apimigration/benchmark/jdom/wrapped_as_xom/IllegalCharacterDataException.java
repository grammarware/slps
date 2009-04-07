package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;


@SuppressWarnings("serial")
@MapsTo("org.jdom.IllegalDataException")
public class IllegalCharacterDataException extends IllegalDataException {

	public IllegalCharacterDataException(String message, String data) {
		super(message, data);
	}

	public IllegalCharacterDataException(org.jdom.IllegalDataException e, String data) {
		super(e, data);
	}


}
