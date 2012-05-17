package org.planet_sl.apimigration.benchmark.dom;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.List;

import org.planet_sl.apimigration.benchmark.dom.wrapped_as_jdom.*;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.ContactsToDocument;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;



public class ContactsToDocumentUsingDOMWrappedAsJDOM extends ContactsToDocument {

	public ContactsToDocumentUsingDOMWrappedAsJDOM(List<Person> contacts) {
		this.contacts = contacts;
	}

	public static void main(String args[]) throws IOException {
		new ContactsToDocumentUsingDOMWrappedAsJDOM(Person.PERSONS).save(System.out);
	}
	
	@Override
	public void save(OutputStream output) throws IOException {
		Writer writer = new PrintWriter(output);
		writer.write(makeDocument().toString());
		writer.flush();
	}
	
	public Document makeDocument() {
		Document document = new Document();			
		Element root = new Element("contacts");		
		document.addContent(root);										
		for (Person p: contacts) {
			Content name = new Element("name");
			name.setText(p.getName());
			Content age = new Element("age");
			age.setText(new Integer(p.getAge()).toString());
			Element person = new Element("person");
			person.addContent(name);
			person.addContent(age);
			root.addContent(person);
		}
		return document;
	}
	
//	public void makeWrapper() {
//		NewDocument document = new NewDocument();
//		NewElement root = new NewElement("contacts");
//		document = new Document$AddContent(document, root);
//		for (Person p: contacts) {
//			NewElement name = new NewElement("name");
//			name = new SetText(name, p.getName());
//			NewElement age = new NewElement("age");
//			name = new SetText(name, new Integer(p.getAge()).toString());
//			person = new NewElement("person");
//			person = new Document$AddContent(person, name);
//			person = new Document$AddContent(person, age);
//			root = new Document$AddContent(root, person);
//		}
//	}
}
