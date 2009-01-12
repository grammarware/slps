package org.planet_sl.apimigration.benchmark.jdom;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

import org.jdom.output.XMLOutputter;
import org.planet_sl.apimigration.benchmark.jdom.wrapped_as_dom.*;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.ContactsToDocument;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;




public class ContactsToDocumentUsingJDOMWrappedAsDOM extends ContactsToDocument {
	
	public ContactsToDocumentUsingJDOMWrappedAsDOM(List<Person> persons) {
		this.contacts = persons;
	}

	public static void main(String args[]) throws IOException {
		new ContactsToDocumentUsingJDOMWrappedAsDOM(Person.PERSONS).save(System.out);
	}
	
	public Document makeDocument() {
		// copy pasted from the DOM implementation
		Document document = getDomImplementation().createDocument(null, "contacts", null); 
		Element root = document.getDocumentElement();
		for (Person p: contacts) {
			Element elt = document.createElement("person");
			Element nameElt = document.createElement("name");
			Node name = document.createTextNode(p.getName());
			nameElt.appendChild(name);
			
			Element ageElt = document.createElement("age");
			Node age = document.createTextNode(new Integer(p.getAge()).toString());
			ageElt.appendChild(age);
			
			elt.appendChild(nameElt);
			elt.appendChild(ageElt);
			
			root.appendChild(elt);
		}
		return document;
	}

	private DOMImplementation getDomImplementation() {
		return new DOMImplementation();
	}

	@Override
	public void save(OutputStream output) throws IOException {
		// This uses JDOM stuff
		Document document = makeDocument();
		XMLOutputter outputter = new XMLOutputter();
		outputter.output(document.doc, output);
	}
}
