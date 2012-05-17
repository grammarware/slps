package org.planet_sl.apimigration.benchmark.scenarios.xo_mapping;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for de.uni_koblenz.using_xml_apis.xo_mapping");
		//$JUnit-BEGIN$
		suite.addTestSuite(TestDocumentToContacts.class);
		suite.addTestSuite(TestContactsToDocument.class);
		//$JUnit-END$
		return suite;
	}

}
