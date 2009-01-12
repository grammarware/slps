package org.planet_sl.apimigration.benchmark.jdom;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.List;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.output.XMLOutputter;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.ContactsToDocument;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;



public class ContactsToDocumentUsingJDOM extends ContactsToDocument {
	public static void main(String args[]) throws IOException {
		new ContactsToDocumentUsingJDOM(Person.PERSONS).save(System.out);
	}

	public ContactsToDocumentUsingJDOM(List<Person> contacts) {
		this.contacts = contacts;
	}
	
	public void save(OutputStream output) throws IOException {
		Document document = makeDocument();
		XMLOutputter outputter = new XMLOutputter();
		outputter.output(document, output);
	}

	public void save14(OutputStream output) throws IOException {
		Document document = makeDocument14();
		XMLOutputter outputter = new XMLOutputter();
		outputter.output(document, output);
	}

	public void saveBottomUp(OutputStream output) throws IOException {
		Document document = makeDocumentBottomUp();
		XMLOutputter outputter = new XMLOutputter();
		outputter.output(document, output);
	}

	
	public Document makeDocument() {
		Document doc = new Document();
		Element root = new Element("contacts");
		doc.addContent(root);
		for (Person p: contacts) {
			
		    // XML element for person
			Element px = new Element("person");
			
		    // Name of person
			Element namex = new Element("name");
			namex = namex.setText(p.getName());
			System.out.println(namex);
			px = px.addContent(namex);
			
		    // Age of person
			Element agex = new Element("age");
			agex = agex.setText(new Integer(p.getAge()).toString());
			System.out.println(agex);
			px = px.addContent(agex);

			root.addContent(px);
		}
		return doc;
	}

	public Document makeDocumentTopDown() {
		Document document = new Document();
		Element root = new Element("contacts");
		document = document.addContent(root);
		for (Person p: contacts) {
			Element person = new Element("person");
			Element name = new Element("name");
			name = name.setText(p.getName());
			person = person.addContent(name);
			Element age = new Element("age");
			age = age.setText(new Integer(p.getAge()).toString());
			person = person.addContent(age);
			root.addContent(person);
		}
		return document;
	}
	
	public Document makeDocumentBottomUp() {
		Element root = new Element("contacts");
		for (Person p: contacts) {
			Element name = new Element("name");
			name = name.setText(p.getName());
			Element age = new Element("age");
			age = age.setText(new Integer(p.getAge()).toString());
			Element person = new Element("person");
			person = person.addContent(name);
			person = person.addContent(age);
			root.addContent(person);
		}
		Document document = new Document();
		document = document.addContent(root);
		return document;
	}


	
	@SuppressWarnings("unchecked")
	public Document makeDocument14() {
		Document document = new Document();
		Element root = new Element("contacts");
		document.addContent(root);
		Iterator iter = contacts.iterator();
		while (iter.hasNext()) {
			Person p = (Person)iter.next();
			Element person = new Element("person");
			Element name = new Element("name");
			name.setText(p.getName());
			person.addContent(name);
			Element age = new Element("age");
			age.setText(new Integer(p.getAge()).toString());
			person.addContent(age);
			root.addContent(person);
		}
		return document;
	}

	
	public Document makeDocument_addContentsAtEnd() {
		Document document = new Document();  					// 1
		Element root = new Element("contacts");					// 2
		document.addContent(root);								// 3 -> 1, 2
		for (Person p: contacts) {								// 4 -> contacts
			Element person = new Element("person");				// 5
			Element name = new Element("name");					// 6
			Element age = new Element("age");					// 7
			
			name.setText(p.getName());							// 8 -> 4, 6
			age.setText(new Integer(p.getAge()).toString());	// 9 -> 4, 7
			
			person.addContent(name);							// 10 -> 5, 6
			person.addContent(age);								// 11 -> 5, 7
			
			root.addContent(person);							// 12 -> 2, 5
		}
		return document;										// 13 -> 1
	}
	
	
	/*
	 * body(?kid) :- Element ?kid = new Element(_), loop(?kid)
	 * loop(?root) :- body(?kid), ?root.addContent(?kid), loop(?root).  
	 */
	
	/*
	 * Specification:
	 * new Document() in Document
	 * new Element(...) in Element
	 * for all doc in Document
	 *      exists root in Element
	 * 		   doc.addContent(root);
	 * for all e in Element:
	 *  	e.addContent(Element) | e.setText(...)
	 */
	
	public Document makeDocument_newDocumentAtEnd() {
		Element root = new Element("contacts");
		for (Person p: contacts) {
			Element person = new Element("person");
			Element name = new Element("name");
			Element age = new Element("age");
			
			name.setText(p.getName());
			age.setText(new Integer(p.getAge()).toString());
			
			person.addContent(name);
			person.addContent(age);
			
			root.addContent(person);
		}
		Document document = new Document();
		document.addContent(root);
		return document;
	}
	


	
}
