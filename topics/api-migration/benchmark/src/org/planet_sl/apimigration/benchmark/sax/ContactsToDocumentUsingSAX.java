package org.planet_sl.apimigration.benchmark.sax;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.List;

import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.ContactsToDocument;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;


public class ContactsToDocumentUsingSAX extends ContactsToDocument {

	public static void main(String args[]) throws IOException {
		new ContactsToDocumentUsingSAX(Person.PERSONS).save(System.out);
	}
	
	public ContactsToDocumentUsingSAX(List<Person> contacts) {
		this.contacts = contacts;
	}
	
	@Override
	public void save(OutputStream output) throws IOException {
		PrintWriter printer = new PrintWriter(output);
		printer.println("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>");
		printer.print("<contacts>");
		for (Person p: contacts) {
			printer.print("<person>");
			printer.print("<name>" + p.getName() + "</name>");
			printer.print("<age>" + p.getAge() + "</age>");
			printer.print("</person>");
			
		}
		printer.print("</contacts>");
		printer.flush();
	}

}
