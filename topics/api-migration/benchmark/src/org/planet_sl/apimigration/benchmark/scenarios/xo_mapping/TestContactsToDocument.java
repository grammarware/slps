package org.planet_sl.apimigration.benchmark.scenarios.xo_mapping;

import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;
import org.planet_sl.apimigration.benchmark.dom.ContactsToDocumentUsingDOM;
import org.planet_sl.apimigration.benchmark.dom.ContactsToDocumentUsingDOMWrappedAsJDOM;
import org.planet_sl.apimigration.benchmark.dom4j.ContactsToDocumentUsingDOM4J;
import org.planet_sl.apimigration.benchmark.jaxb.ContactsToDocumentUsingJAXB;
import org.planet_sl.apimigration.benchmark.jaxb.contacts.XContacts;
import org.planet_sl.apimigration.benchmark.jdom.ContactsToDocumentUsingJDOM;
import org.planet_sl.apimigration.benchmark.jdom.ContactsToDocumentUsingJDOMWrappedAsDOM;
import org.planet_sl.apimigration.benchmark.sax.ContactsToDocumentUsingSAX;
import org.planet_sl.apimigration.benchmark.scenarios.accessing.Resources;
import org.planet_sl.apimigration.benchmark.xom.ContactsToDocumentUsingXOM;



public class TestContactsToDocument extends TestCase {

	
	private ContactsToDocumentUsingDOM  domImpl;
	private ContactsToDocumentUsingDOM4J dom4jImpl;
	private ContactsToDocumentUsingJAXB jaxbImpl;
	private ContactsToDocumentUsingJDOM jdomImpl;
	private ContactsToDocumentUsingSAX saxImpl;
	private ContactsToDocumentUsingXOM xomImpl;
	private ContactsToDocumentUsingJDOMWrappedAsDOM jdomDomImpl;
	private ContactsToDocumentUsingDOMWrappedAsJDOM domJDomImpl;
	
	private static List<Person> testData = Person.PERSONS;
	
	private Unmarshaller unmarshaller;
	
	@Before
	public void setUp() throws Exception {
		JAXBContext context = JAXBContext.newInstance(Resources.JAXB_PACKAGE + ".contacts");
		unmarshaller = context.createUnmarshaller();
		domImpl = new ContactsToDocumentUsingDOM(testData);
		dom4jImpl = new ContactsToDocumentUsingDOM4J(testData);
		jaxbImpl = new ContactsToDocumentUsingJAXB(testData);
		jdomImpl = new ContactsToDocumentUsingJDOM(testData);
		saxImpl = new ContactsToDocumentUsingSAX(testData);
		xomImpl = new ContactsToDocumentUsingXOM(testData);
		jdomDomImpl = new ContactsToDocumentUsingJDOMWrappedAsDOM(testData);
		domJDomImpl = new ContactsToDocumentUsingDOMWrappedAsJDOM(testData);
	}
	
	private boolean contactsEqualToTestData(XContacts contacts) {
		int i = 0;
		for (org.planet_sl.apimigration.benchmark.jaxb.contacts.XPerson xmlPerson: contacts.getPerson()) {
			if (!xmlPerson.getName().equals(testData.get(i).getName())) {
				return false;
			}
			if (xmlPerson.getAge() != (testData.get(i).getAge())) {
				return false;
			}
			i++;
		}
		return true;
	}
	
	private XContacts contactsFromImpl(ContactsToDocument impl) throws IOException, JAXBException {
		PipedOutputStream output = new PipedOutputStream();
		PipedInputStream input = new PipedInputStream();
		output.connect(input);
		impl.save(output);
		output.close();
		// Note: this probably only works because the XML term is so small
		// The documentation on Piped* states there is a risk of deadlock
		// when writing/reading from the same thread.
		return (XContacts) unmarshaller.unmarshal(input);
	}
	
	@Test
	public void testDOM() throws IOException, JAXBException {		
		Assert.assertTrue(contactsEqualToTestData(contactsFromImpl(domImpl)));
	}
	
	@Test
	public void testDOM4J() throws IOException, JAXBException {		
		Assert.assertTrue(contactsEqualToTestData(contactsFromImpl(dom4jImpl)));
	}
	
	@Test
	public void testJAXB() throws IOException, JAXBException {		
		Assert.assertTrue(contactsEqualToTestData(contactsFromImpl(jaxbImpl)));
	}
	
	@Test
	public void testJDOM() throws IOException, JAXBException {		
		Assert.assertTrue(contactsEqualToTestData(contactsFromImpl(jdomImpl)));
	}
	
	@Test
	public void testJDOMBottomUp() throws IOException, JAXBException {		
		PipedOutputStream output = new PipedOutputStream();
		PipedInputStream input = new PipedInputStream();
		output.connect(input);
		jdomImpl.saveBottomUp(output);
		output.close();
		Assert.assertTrue(contactsEqualToTestData((XContacts) unmarshaller.unmarshal(input)));
	}
	
	
	@Test
	public void testJDOM14() throws IOException, JAXBException {		
		PipedOutputStream output = new PipedOutputStream();
		PipedInputStream input = new PipedInputStream();
		output.connect(input);
		jdomImpl.save14(output);
		output.close();
		Assert.assertTrue(contactsEqualToTestData((XContacts) unmarshaller.unmarshal(input)));
	}
	
	@Test
	public void testSAX() throws IOException, JAXBException {		
		Assert.assertTrue(contactsEqualToTestData(contactsFromImpl(saxImpl)));
	}
	
	@Test
	public void testXOM() throws IOException, JAXBException {		
		Assert.assertTrue(contactsEqualToTestData(contactsFromImpl(xomImpl)));
	}
	
	@Test
	public void testJDOMwrappedAsDOM() throws IOException, JAXBException {		
		Assert.assertTrue(contactsEqualToTestData(contactsFromImpl(jdomDomImpl)));
	}
	
	@Test
	public void testDOMwrappedAsJDOM() throws IOException, JAXBException {		
		Assert.assertTrue(contactsEqualToTestData(contactsFromImpl(domJDomImpl)));
	}
	
}
