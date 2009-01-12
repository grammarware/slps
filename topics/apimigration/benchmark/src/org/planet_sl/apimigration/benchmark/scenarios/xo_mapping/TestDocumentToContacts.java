package org.planet_sl.apimigration.benchmark.scenarios.xo_mapping;


import java.util.List;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;
import org.planet_sl.apimigration.benchmark.dom.ContactsToDocumentUsingDOM;
import org.planet_sl.apimigration.benchmark.dom.ContactsToDocumentUsingDOMWrappedAsJDOM;
import org.planet_sl.apimigration.benchmark.dom.DocumentToContactsUsingDOM;
import org.planet_sl.apimigration.benchmark.dom.DocumentToContactsUsingDOMWrappedAsJDOM;
import org.planet_sl.apimigration.benchmark.jdom.ContactsToDocumentUsingJDOM;
import org.planet_sl.apimigration.benchmark.jdom.ContactsToDocumentUsingJDOMWrappedAsDOM;
import org.planet_sl.apimigration.benchmark.jdom.DocumentToContactsUsingJDOM;
import org.planet_sl.apimigration.benchmark.jdom.DocumentToContactsUsingJDOMWrappedAsDOM;
import org.planet_sl.apimigration.benchmark.sax.DocumentToContactsUsingSAX;


public class TestDocumentToContacts extends TestCase {

	private static final String CONTACTS_XML = "resources/contacts.xml";

	private List<Person> testData;

	private DocumentToContactsUsingJDOM jdomImpl;
	private DocumentToContactsUsingDOM domImpl;
	private DocumentToContactsUsingSAX saxImpl;
	private DocumentToContactsUsingJDOMWrappedAsDOM jdomAsDomImpl;
	private DocumentToContactsUsingDOMWrappedAsJDOM domAsJDomImpl;
	
	@Before
	public void setUp() throws Exception {
		testData = Person.PERSONS;
		jdomImpl = new DocumentToContactsUsingJDOM(new ContactsToDocumentUsingJDOM(Person.PERSONS).makeDocument());
		domImpl = new DocumentToContactsUsingDOM(new ContactsToDocumentUsingDOM(Person.PERSONS).makeDocument());
		saxImpl = new DocumentToContactsUsingSAX(CONTACTS_XML);
		jdomAsDomImpl = new DocumentToContactsUsingJDOMWrappedAsDOM(new ContactsToDocumentUsingJDOMWrappedAsDOM(Person.PERSONS).makeDocument());
		domAsJDomImpl = new DocumentToContactsUsingDOMWrappedAsJDOM(new ContactsToDocumentUsingDOMWrappedAsJDOM(Person.PERSONS).makeDocument());
	}
	
	@Test
	public void testJDOM() {
		Assert.assertEquals(testData, jdomImpl.makeContacts());
	}
	
	@Test
	public void testDOM() {
		Assert.assertEquals(testData, domImpl.makeContacts());
	}
	
	@Test
	public void testSAX() {
		Assert.assertEquals(testData, saxImpl.makeContacts());
	}

	@Test
	public void testJDOMwrappedAsDOM() {
		Assert.assertEquals(testData, jdomAsDomImpl.makeContacts());
	}
	
	@Test
	public void testDOMwrappedAsJDOM() {
		Assert.assertEquals(testData, domAsJDomImpl.makeContacts());
	}
	
	
}
