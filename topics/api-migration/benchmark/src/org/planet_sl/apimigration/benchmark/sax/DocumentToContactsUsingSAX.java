package org.planet_sl.apimigration.benchmark.sax;

import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.DocumentToContacts;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;


public class DocumentToContactsUsingSAX extends DocumentToContacts {

	private String filename;

	public DocumentToContactsUsingSAX(String filename) {
		this.filename = filename;
	}
	
	private static class ContactsBuilder extends DefaultHandler {
		private boolean isName = false;
		private boolean isAge= false;
		private String name = null;
		private List<Person> contacts = new LinkedList<Person>();

		public void startElement(String uri, String name, String qName, Attributes atts) {
			isName = name.equals("name");
			isAge = name.equals("age");
		}
		public void endElement(String uri, String name, String qName) {
			isName = false;
			isAge = false;
		}

		public void characters(char ch[], int start, int length) {
			String str = String.valueOf(ch, start, length);
			if (isName) {
				name = str;
			}
			if (isAge) {
				int age = Integer.parseInt(str);
				contacts.add(new Person(name, age));
			}
		}
		
	}

	
	@Override
	public List<Person> makeContacts() {
		try {
			XMLReader reader = XMLReaderFactory.createXMLReader();
			ContactsBuilder handler = new ContactsBuilder();
			reader.setContentHandler(handler);
			reader.setErrorHandler(handler);
			reader.parse(new InputSource(new FileReader(filename)));
			return handler.contacts;
		}
		catch (SAXException e) {
			throw new RuntimeException("sax exception");
		}
		catch (IOException e) {
			throw new RuntimeException("io exception");
		}
	}

}
