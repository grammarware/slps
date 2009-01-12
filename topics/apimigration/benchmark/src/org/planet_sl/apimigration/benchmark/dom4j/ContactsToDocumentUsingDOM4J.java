package org.planet_sl.apimigration.benchmark.dom4j;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

import org.dom4j.Document;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.io.XMLWriter;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.ContactsToDocument;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;


public class ContactsToDocumentUsingDOM4J extends ContactsToDocument {
	public static void main(String args[]) throws IOException {
		new ContactsToDocumentUsingDOM4J(Person.PERSONS).save(System.out);
	}
	
	public ContactsToDocumentUsingDOM4J(List<Person> contacts) {
		this.contacts = contacts;
	}
	
	public void save(OutputStream output) throws IOException {
		Document document = makeDocument();
        XMLWriter writer = new XMLWriter(output);
        writer.write(document);
	}

	public Document makeDocument() {
		Document document = DocumentHelper.createDocument();
		Element root = document.addElement("contacts");
		for (Person person: contacts) {
			Element elt = root.addElement("person");
			elt.addElement("name").addText(person.getName());
			elt.addElement("age").addText(new Integer(person.getAge()).toString());
		}
		return document;
	}


	
}
