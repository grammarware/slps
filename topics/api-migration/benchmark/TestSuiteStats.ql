/**
 * @name TestSuiteStats
 * @kind table
 */
import default

from Class testSuite, int testMethods, int assertions, boolean ignored, int avgAssertionsPerTest
where 
	testSuite.getPackage().getName() = "nu.xom.tests" 
  and testSuite.getASupertype().getName() = "XOMTestCase"
  and (
		if (testSuite.getName() = "AttributeTest" or
			testSuite.getName() = "BuilderTest" or
			testSuite.getName() = "CommentTest" or
			testSuite.getName() = "DocTypeTest" or
			testSuite.getName() = "DocumentTest" or
			testSuite.getName() = "ElementTest" or
			testSuite.getName() = "NodesTest" or
			testSuite.getName() = "ParentNodeTest" or
			testSuite.getName() = "ProcessingInstructionTest" or
			testSuite.getName() = "TextTest" or
			testSuite.getName() = "NamespacesTest" or
			testSuite.getName() = "NodesTest" or
			testSuite.getName() = "LeafNodeTest" or
			testSuite.getName() = "BaseURITest")
     then 
			ignored = false 
		else ignored = true)
  and testMethods = count(Method m | 
				testSuite.contains(m) and m.getName().matches("test%") )
  and assertions = count(Call call | 
				testSuite.contains(call.getCaller()) and
					call.getCallee().getName().matches("assert%") and
					call.getCaller().getName().matches("test%"))
  and avgAssertionsPerTest = assertions / testMethods

select ignored, testSuite, testMethods, assertions , avgAssertionsPerTest