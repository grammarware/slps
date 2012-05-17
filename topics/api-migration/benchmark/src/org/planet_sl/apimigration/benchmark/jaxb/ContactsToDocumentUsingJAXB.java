package org.planet_sl.apimigration.benchmark.jaxb;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.planet_sl.apimigration.benchmark.jaxb.contacts.*;
import org.planet_sl.apimigration.benchmark.scenarios.accessing.Resources;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.ContactsToDocument;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;


public class ContactsToDocumentUsingJAXB extends ContactsToDocument {
	private ObjectFactory factory;
	
	public static void main(String args[]) throws IOException, JAXBException {
		new ContactsToDocumentUsingJAXB(Person.PERSONS).save(System.out);
	}
	
	public ContactsToDocumentUsingJAXB(List<Person> contacts) {
		this.contacts = contacts;
		this.factory = new ObjectFactory();
	}
	
	public void save(OutputStream output) throws IOException {
		XContacts document = makeDocument();
		try {
			JAXBContext context = JAXBContext.newInstance(Resources.JAXB_PACKAGE + ".contacts");
			Marshaller marshaller = context.createMarshaller();
			marshaller.marshal(document, output);
		}
		catch (JAXBException e) {
			e.printStackTrace();
		}
	}

	private XContacts makeDocument() {
		XContacts doc = factory.createContacts();
		for (Person p: contacts) {
			XPerson px = new XPerson();
			px.setName(p.getName());
			px.setAge(p.getAge());
			doc.getPerson().add(px);
		}
		return doc;
	}
	
	
	
}
