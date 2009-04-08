package org.planet_sl.apimigration.benchmark.anno;

import junit.framework.TestSuite;
import junit.textui.TestRunner;

import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.AttributeTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.AttributeTypeTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.BaseURITest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.BuilderTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.CommentTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.DocTypeTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.DocumentTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.ElementTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.EncodingTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.IDTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.LeafNodeTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.NamespaceNodeTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.NamespacesTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.NodesTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.ParentNodeTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.ProcessingInstructionTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.TextTest;
import org.planet_sl.apimigration.benchmark.jdom.test_as_xom.XPathTest;

public class RunTest {
	public static void main(String args[]) {

		TestSuite suite = new TestSuite();
		suite.addTestSuite(AttributeTest.class);
//		suite.addTestSuite(AttributeTypeTest.class);
//		suite.addTestSuite(BaseURITest.class);
		suite.addTestSuite(BuilderTest.class);
		suite.addTestSuite(CommentTest.class);
		suite.addTestSuite(DocTypeTest.class);
		suite.addTestSuite(DocumentTest.class);
		suite.addTestSuite(ElementTest.class);
//		suite.addTestSuite(EncodingTest.class);
//		suite.addTestSuite(IDTest.class);
		suite.addTestSuite(LeafNodeTest.class);
//		suite.addTestSuite(NamespaceNodeTest.class);
		suite.addTestSuite(NamespacesTest.class);
		suite.addTestSuite(NodesTest.class);
		suite.addTestSuite(ParentNodeTest.class);
		suite.addTestSuite(ProcessingInstructionTest.class);
		suite.addTestSuite(TextTest.class);
//		suite.addTestSuite(XPathTest.class);
		TestRunner runner = new TestRunner();
		runner.doRun(suite);
	}
}
