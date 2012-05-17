package org.planet_sl.apimigration.benchmark.anno;

import java.util.HashMap;
import java.util.Map;

import junit.framework.TestSuite;
import junit.textui.TestRunner;


import org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.*;

@SuppressWarnings("unused")
public aspect FailingWrappedMethods {

	static int tests = 0;
	// TODO: we need a relation type here; not a dictionary.
	static Map<String, String> failures = new HashMap<String, String>();
	static Map<String, String> errors = new HashMap<String, String>();
	static Map<String, RuntimeException> exceptions = new HashMap<String, RuntimeException>();
	static String currentTest = null;
	static String currentMethod = null;
	
	after(): execution(public static void CollectFailures.main(..)) {
		System.out.println("***** FAILURES ****** (" + failures.size() + " out of " + tests + ")");
		int i = 1;
		for (String test: failures.keySet()) {
			String prettyTest = test.replaceAll("execution\\(", "").replaceAll("\\)$", "");
			String prettyMethod = failures.get(test);
			prettyMethod = prettyMethod.replaceAll("execution\\(", "");
			prettyMethod = prettyMethod.replaceAll("\\)$", "");
			prettyMethod = prettyMethod.replaceAll("org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.", "");
			prettyMethod = prettyMethod.substring(prettyMethod.indexOf(" ") + 1);
			System.out.println(i + " " + prettyTest + ": " + prettyMethod);
			i++;
		}
		System.out.println("***** ERRORS ****** (" + errors.size() + " out of " + tests + ")");
		// TODO refactor...
		i = 1;
		for (String test: errors.keySet()) {
			String prettyTest = test.replaceAll("execution\\(", "").replaceAll("\\)$", "");
			String prettyMethod = errors.get(test);
			prettyMethod = prettyMethod.replaceAll("execution\\(", "");
			prettyMethod = prettyMethod.replaceAll("\\)$", "");
			prettyMethod = prettyMethod.replaceAll("org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.", "");
			prettyMethod = prettyMethod.substring(prettyMethod.indexOf(" ") + 1);
			System.out.println(i + " " + prettyTest + ": " + prettyMethod + " ("
					+ exceptions.get(test).getClass() + ")");
			i++;
		}
	}
	
	void around(): execution(void test*()) {
		tests++;
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
			exceptions.put(currentTest, e);
			throw e;
		}
	}
	
	before(): execution(* org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.*.*(..)) {
//		System.err.println("Executing of wrapper: " + thisJoinPoint.toShortString());
		currentMethod = thisJoinPoint.toString();
	}
	
	
}
