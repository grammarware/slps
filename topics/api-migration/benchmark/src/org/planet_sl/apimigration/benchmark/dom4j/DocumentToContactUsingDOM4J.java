package org.planet_sl.apimigration.benchmark.dom4j;


import java.util.ArrayList;
import java.util.List;

import org.dom4j.Document;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.DocumentToContacts;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;

public class DocumentToContactUsingDOM4J extends DocumentToContacts {

	private Document document;
	
	public DocumentToContactUsingDOM4J(Document document) {
		this.document = document;
	}

	@Override
	public List<Person> makeContacts() {
		List<Person> persons = new ArrayList<Person>();
		
		return null;
	}
	
}
