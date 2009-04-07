package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;


@SuppressWarnings("serial")
@MapsTo("org.jdom.IllegalAddException")
public class MultipleParentException extends IllegalAddException {

	public MultipleParentException(org.jdom.IllegalAddException e) {
		super(e);
	}

	public MultipleParentException(String string) {
		super(string);
	}

}
