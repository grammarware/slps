package org.planet_sl.apimigration.benchmark.analysis;

import junit.framework.TestSuite;
import junit.textui.TestRunner;
//import nu.xom.tests.AttributeTest;
//import nu.xom.tests.AttributeTypeTest;
//import nu.xom.tests.BaseURITest;
//import nu.xom.tests.BuilderTest;
//import nu.xom.tests.CommentTest;
//import nu.xom.tests.DocTypeTest;
//import nu.xom.tests.DocumentTest;
//import nu.xom.tests.ElementTest;
//import nu.xom.tests.LeafNodeTest;
//import nu.xom.tests.NamespacesTest;
//import nu.xom.tests.NodesTest;
//import nu.xom.tests.ParentNodeTest;
//import nu.xom.tests.ProcessingInstructionTest;
//import nu.xom.tests.TextTest;

public class CollectFailuresXOM {

	public static void main(String args[]) {
		TestSuite suite = new TestSuite();
//		suite.addTestSuite(AttributeTest.class);
//		suite.addTestSuite(AttributeTypeTest.class);
//		suite.addTestSuite(BaseURITest.class);
//		suite.addTestSuite(BuilderTest.class);
//		suite.addTestSuite(CommentTest.class);
//		suite.addTestSuite(DocTypeTest.class);
//		suite.addTestSuite(DocumentTest.class);
//		suite.addTestSuite(ElementTest.class);
//		suite.addTestSuite(LeafNodeTest.class);
//		//			suite.addTestSuite(NamespaceNodeTest.class);
//		suite.addTestSuite(NamespacesTest.class);
//		suite.addTestSuite(NodesTest.class);
//		suite.addTestSuite(ParentNodeTest.class);
//		suite.addTestSuite(ProcessingInstructionTest.class);
//		suite.addTestSuite(TextTest.class);
		TestRunner runner = new TestRunner();
		runner.doRun(suite);
	}

}
