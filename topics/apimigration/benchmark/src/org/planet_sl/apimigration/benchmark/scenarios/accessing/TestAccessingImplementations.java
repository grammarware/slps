package org.planet_sl.apimigration.benchmark.scenarios.accessing;


import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;
import org.planet_sl.apimigration.benchmark.dom.AccessingContactsUsingDOM;
import org.planet_sl.apimigration.benchmark.dom.AccessingSalariesUsingDOM;
import org.planet_sl.apimigration.benchmark.dom.AccessingSalariesUsingDOMWrappedAsJDOM;
import org.planet_sl.apimigration.benchmark.dom4j.AccessingSalariesUsingDOM4J;
import org.planet_sl.apimigration.benchmark.jaxb.AccessingSalariesUsingJAXB;
import org.planet_sl.apimigration.benchmark.jdom.AccessingContactsUsingJDOM;
import org.planet_sl.apimigration.benchmark.jdom.AccessingSalariesUsingJDOM;
import org.planet_sl.apimigration.benchmark.sax.AccessingSalariesUsingSAX;


public class TestAccessingImplementations {

	private static final int EXPECTED_TOTAL = 399747;

	private static final int EXPECTED_TOTAL_AFTER_RAISE = 399754;

	protected AbstractIncreaseSalaries increaser;

	AccessingSalariesUsingDOM domImpl;
	AccessingSalariesUsingDOMWrappedAsJDOM domJDomImpl;
	AccessingSalariesUsingDOM4J dom4jImpl;
	AccessingSalariesUsingJAXB jaxbImpl;
	AccessingSalariesUsingJDOM jdomImpl;
	AccessingSalariesUsingSAX saxImpl;
	AccessingContactsUsingDOM domContactsImpl;
	AccessingContactsUsingJDOM jdomContactsImpl;
	
	private static final String testFile = Resources.COMPANY_XML;
	private static final String contactsFile = Resources.CONTACTS_XML;
	
	private static final float EXPECTED_AVERAGE_AGE = 59.5f;

	private static final Object AVERAGE_AGE_AFTER_AUGUST29 = 60f;
	
	@Before
	public void setUp() throws Exception {
		domImpl = new AccessingSalariesUsingDOM(testFile);
		domJDomImpl = new AccessingSalariesUsingDOMWrappedAsJDOM(testFile);
		dom4jImpl = new AccessingSalariesUsingDOM4J(testFile);
		jaxbImpl = new AccessingSalariesUsingJAXB(testFile);
		jdomImpl = new AccessingSalariesUsingJDOM(testFile);
		saxImpl = new AccessingSalariesUsingSAX(testFile);
		domContactsImpl = new AccessingContactsUsingDOM(contactsFile);
		jdomContactsImpl = new AccessingContactsUsingJDOM(contactsFile);
	}
	

	// TODO: figure out how to generate these tests (i.e. w/o having to duplicate them)
	
	
	@Test
	public void testAugust29UsingJDom() {
		jdomContactsImpl.august29();
		Assert.assertEquals(AVERAGE_AGE_AFTER_AUGUST29, jdomContactsImpl.average());
	}
	
	@Test
	public void testAugust29UsingDom() {
		domContactsImpl.august29();
		Assert.assertEquals(AVERAGE_AGE_AFTER_AUGUST29, domContactsImpl.average());
	}
	
	
	@Test
	public void testAverageAgeUsingDom() {
		Assert.assertEquals(EXPECTED_AVERAGE_AGE, domContactsImpl.average());
	}
	
	@Test
	public void testAverageAgeUsingJDom() {
		Assert.assertEquals(EXPECTED_AVERAGE_AGE, jdomContactsImpl.average());
	}
	
	@Test
	public void testSumUsingDOM() {
		double total = domImpl.sum();
		Assert.assertEquals(EXPECTED_TOTAL, total, 0.0);
	}

	@Test
	public void testRaiseUsingDOM() {
		domImpl.raise(1.0);
		double total = domImpl.sum();
		Assert.assertEquals(EXPECTED_TOTAL_AFTER_RAISE, total, 0.0);
	}

	@Test
	public void testSumUsingDOMwrappedAsJDOM() {
		double total = domJDomImpl.sum();
		Assert.assertEquals(EXPECTED_TOTAL, total, 0.0);
	}

	@Test
	public void testRaiseUsingDOMwrappedAsJDOM() {
		domImpl.raise(1.0);
		double total = domJDomImpl.sum();
		Assert.assertEquals(EXPECTED_TOTAL_AFTER_RAISE, total, 0.0);
	}

	@Test
	public void testSumUsingDOM4J() {
		double total = dom4jImpl.sum();
		Assert.assertEquals(EXPECTED_TOTAL, total, 0.0);
	}

	@Test
	public void testRaiseUsingDom4J() {
		dom4jImpl.raise(1.0);
		double total = dom4jImpl.sum();
		Assert.assertEquals(EXPECTED_TOTAL_AFTER_RAISE, total, 0.0);
	}

	@Test
	public void testSumUsingJAXB() {
		double total = jaxbImpl.sum();
		Assert.assertEquals(EXPECTED_TOTAL, total, 0.0);
	}

	@Test
	public void testRaiseUsingJAXB() {
		jaxbImpl.raise(1.0);
		double total = jaxbImpl.sum();
		Assert.assertEquals(EXPECTED_TOTAL_AFTER_RAISE, total, 0.0);
	}

	@Test
	public void testSumUsingJDOM() {
		double total = jdomImpl.sum();
		Assert.assertEquals(EXPECTED_TOTAL, total, 0.0);
	}

	@Test
	public void testRaiseUsingJDom() {
		jdomImpl.raise(1.0);
		double total = jdomImpl.sum();
		Assert.assertEquals(EXPECTED_TOTAL_AFTER_RAISE, total, 0.0);
	}
	
	@Test
	public void testSumUsingSAX() {
		double total = saxImpl.sum();
		Assert.assertEquals(EXPECTED_TOTAL, total, 0.0);
	}

}
