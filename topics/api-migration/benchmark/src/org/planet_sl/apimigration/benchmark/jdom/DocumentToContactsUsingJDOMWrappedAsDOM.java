package org.planet_sl.apimigration.benchmark.jdom;

import java.util.LinkedList;
import java.util.List;

import org.planet_sl.apimigration.benchmark.jdom.wrapped_as_dom.*;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.DocumentToContacts;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;



public class DocumentToContactsUsingJDOMWrappedAsDOM extends DocumentToContacts {

	private Document document;
	
	public DocumentToContactsUsingJDOMWrappedAsDOM(Document document) {
		this.document = document;
	}
	
	public static void main(String arg[]) {
		System.out.println(
				new DocumentToContactsUsingJDOMWrappedAsDOM(new ContactsToDocumentUsingJDOMWrappedAsDOM(Person.PERSONS).makeDocument())
				.makeContacts());
	}
	
	
	@Override
	public List<Person> makeContacts() {
		// Copy pasted from DOM implementation
		LinkedList<Person> contacts = new LinkedList<Person>();
		NodeList persons = document.getElementsByTagName("person");
		for (int i = 0; i < persons.getLength(); i++) {
			Element elem = (Element) persons.item(i);
			Element name = (Element) elem.getFirstChild();
			Element age = (Element) name.getNextSibling();
			contacts.add(new Person(name.getTextContent(), Integer.parseInt(age.getTextContent())));
		}
		return contacts;
	}

}
