package org.planet_sl.apimigration.benchmark.dom;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.planet_sl.apimigration.benchmark.dom.wrapped_as_jdom.*;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.DocumentToContacts;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;



public class DocumentToContactsUsingDOMWrappedAsJDOM extends DocumentToContacts {
	private Document document;
	
	public static void main(String args[]) {
		System.out.println(
				new DocumentToContactsUsingDOMWrappedAsJDOM(
						new ContactsToDocumentUsingDOMWrappedAsJDOM(Person.PERSONS).makeDocument()).makeContacts());
	}
	
	public DocumentToContactsUsingDOMWrappedAsJDOM(Document document) {
		this.document = document;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Person> makeContacts() {
		LinkedList<Person> contacts = new LinkedList<Person>();
		Iterator iter = document.getDescendants();
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
