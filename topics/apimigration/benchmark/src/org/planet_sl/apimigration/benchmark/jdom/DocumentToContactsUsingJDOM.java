package org.planet_sl.apimigration.benchmark.jdom;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.jdom.Document;
import org.jdom.Element;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.DocumentToContacts;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;


public class DocumentToContactsUsingJDOM extends DocumentToContacts {

	private Document document;

	public DocumentToContactsUsingJDOM(Document document) {
		this.document = document;
	}

	public static void main(String args[]) {
		System.out.println(new DocumentToContactsUsingJDOM(new ContactsToDocumentUsingJDOM(Person.PERSONS).makeDocument()).makeContacts());
	}
	
	@SuppressWarnings("unchecked")
	public List<Person> makeContacts() {
		LinkedList<Person> contacts = new LinkedList<Person>();
		Iterator<Object> iter = document.getDescendants();
		while (iter.hasNext()) {
			Object node = iter.next();
			if (node instanceof Element) {
				Element elt = (Element)node;
				if (elt.getName().equals("person")) {
					String name = elt.getChild("name").getText();
					Integer age = Integer.parseInt(elt.getChild("age").getText());
					contacts.add(new Person(name, age));
				}
			}
		}
		return contacts;
	}
	
}
