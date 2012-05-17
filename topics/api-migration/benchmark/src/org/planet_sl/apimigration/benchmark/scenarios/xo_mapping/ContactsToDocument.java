package org.planet_sl.apimigration.benchmark.scenarios.xo_mapping;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

public abstract class ContactsToDocument extends Base {
	protected List<Person> contacts;
	
	public abstract void save(OutputStream output) throws IOException;
	
	
}
