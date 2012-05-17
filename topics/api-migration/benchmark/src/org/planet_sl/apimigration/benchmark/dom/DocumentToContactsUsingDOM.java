package org.planet_sl.apimigration.benchmark.dom;

import java.util.LinkedList;
import java.util.List;

import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.DocumentToContacts;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


public class DocumentToContactsUsingDOM extends DocumentToContacts {

	private Document document;

	public DocumentToContactsUsingDOM(Document document) {
		this.document = document;
	}

	public static void main(String arg[]) {
		ContactsToDocumentUsingDOM domImpl = new ContactsToDocumentUsingDOM(Person.PERSONS);
		Document document = domImpl.makeDocument();
		System.out.println(new DocumentToContactsUsingDOM(document).makeContacts());
	}
	
	@Override
	public List<Person> makeContacts() {
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
