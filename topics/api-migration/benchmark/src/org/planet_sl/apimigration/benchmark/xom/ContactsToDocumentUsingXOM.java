package org.planet_sl.apimigration.benchmark.xom;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.ContactsToDocument;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;

import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Serializer;


public class ContactsToDocumentUsingXOM extends ContactsToDocument {

	public static void main(String args[]) throws IOException {
		new ContactsToDocumentUsingXOM(Person.PERSONS).save(System.out);
	}
	
	public ContactsToDocumentUsingXOM(List<Person> contacts) {
		this.contacts = contacts;
	}
	
	@Override
	public void save(OutputStream output) throws IOException {
		Document document = makeDocument();
		Serializer serializer = new Serializer(output);
		serializer.write(document);
	}
	
	public Document makeDocument() {
		Element root = new Element("contacts");
		for (Person p: contacts) {
			Element person = new Element("person");
			Element age = new Element("age");
			Element name = new Element("name");
			name.appendChild(p.getName());
			age.appendChild(new Integer(p.getAge()).toString());
			person.appendChild(name);
			person.appendChild(age);
			root.appendChild(person);
		}
		return new Document(root);
	}

}
