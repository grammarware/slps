package org.planet_sl.apimigration.benchmark.anno;

import java.util.HashMap;
import java.util.Map;

import junit.framework.TestSuite;
import junit.textui.TestRunner;


import org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.*;

@SuppressWarnings("unused")
public aspect FailingWrappedMethods {

	static Map<String, String> failures = new HashMap<String, String>();
	static Map<String, String> errors = new HashMap<String, String>();
	static String currentTest = null;
	static String currentMethod = null;
	
	after(): execution(public static void CollectFailures.main(..)) {
		System.out.println("***** FAILURES ******");
		for (String test: failures.keySet()) {
			System.out.println(test + ": " + failures.get(test));
		}
		System.out.println("***** ERRORS ******");
		for (String test: errors.keySet()) {
			System.out.println(test + ": " + errors.get(test));
		}
	}
	
	void around(): execution(void test*()) {
		try {
			currentTest = thisJoinPoint.toShortString();
			proceed();
		}
		catch (AssertionError e) {
			failures.put(currentTest, currentMethod);
			throw e;
		}
		catch (RuntimeException e) {
			errors.put(currentTest, currentMethod);
			throw e;
		}
	}
	
	before(): execution(* org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.*.*(..)) {
//		System.err.println("Executing of wrapper: " + thisJoinPoint.toShortString());
		currentMethod = thisJoinPoint.toShortString();
	}
	
	
}
