package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.jdom.IllegalAddException;
import org.jdom.IllegalNameException;

@SuppressWarnings("serial")
public class NamespaceConflictException extends RuntimeException {

	public NamespaceConflictException(IllegalNameException e, String uri) {
		super(e.getMessage());
	}

	public NamespaceConflictException(String string) {
		super(string);
	}

	public NamespaceConflictException(IllegalAddException e) {
		super(e.getMessage());
	}

}
