package org.planet_sl.apimigration.benchmark.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Collections;

import junit.framework.TestSuite;
import junit.textui.TestRunner;


import org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.*;

@SuppressWarnings("unused")
public aspect InterceptTestFailure {

	static int tests = 0;
	static int assertionsTotal = 0;
	static int failuresTotal = 0;
	static int errorsTotal = 0;
	static String currentTest = null;
	static String currentMethod = null;
	
	private static String prettyName(String name) {
		name = name.replaceAll("execution\\(\\1\\)$", "\\1");
		name = name.replaceAll("\\)$", "");
		name = name.replaceAll("org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.", "");
		name = name.substring(name.indexOf(" ") + 1);
		return name;
	}
	
	after(): execution(public static void CollectFailures.main(..)) {
		System.out.println(tests + " tests with " + assertionsTotal + " assertions");
		System.out.println(failuresTotal + " failures; " + errorsTotal + " errors");
		for (String method: assertionCount.keySet()) {
			System.out.println("assertion of " + method + ": " + assertionCount.get(method));
		}
		for (String method: failureCount.keySet()) {
			System.out.println("failures of " + method + ": " + failureCount.get(method));
		}
		for (String method: errorCount.keySet()) {
			System.out.println("errors of " + method + ": " + errorCount.get(method));
		}
		for (String method: successCount.keySet()) {
			System.out.println("successes of " + method + ": " + successCount.get(method));
		}
	}
	
	private static Map<String,Integer> failureCount = new HashMap<String, Integer>();
	private static Map<String,Integer> errorCount = new HashMap<String, Integer>();
	private static Map<String,Integer> assertionCount = new HashMap<String, Integer>();
	private static Map<String,Integer> successCount = new HashMap<String, Integer>();
	private static java.util.List<String> methods = new ArrayList<String>();
	private static int assertions = 0;

	private static void inc(Map<String,Integer> table, String currentMethod) {
		inc(table, currentMethod, 1);
	}
	
	private static void inc(Map<String,Integer> table, String currentMethod, int count) {
		if (!table.containsKey(currentMethod)) {
			table.put(currentMethod, 0);
		}
		table.put(currentMethod, table.get(currentMethod) + count);
	}
	
	before(): call(void assert*(..)) && within(org.planet_sl.apimigration.benchmark.jdom.test_as_xom.*) {
		assertions++;
		assertionsTotal++;
	}
	
	private static boolean wasFeature = false;
	private static Map<String,Integer> featureCallCount = new HashMap<String, Integer>();
	
	after(): execution(* org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.*.*(..)) {
		currentMethod = thisJoinPoint.toShortString();
		inc(featureCallCount, currentMethod);
		if (!methods.contains(currentMethod)) {
			methods.add(currentMethod);
		}
	}

	
	void around(): execution(void test*()) && within(org.planet_sl.apimigration.benchmark.jdom.test_as_xom.*) {
		methods.clear();
		assertions = 0;
		tests++;
		try {
			proceed();
		}
		catch (AssertionError e) {
			failuresTotal++;
			for (String method: methods) {
				inc(failureCount, method);
			}
			throw e;
		}
		catch (Throwable e) {
			errorsTotal++;
			for (String method: methods) {
				inc(errorCount, method);
			}
			//throw e;
		}
		for (String method: methods) {
			inc(successCount, method, assertions);
		}
	}
	

//	Collections.sort(methods);
//	for (String method: methods) {
//		System.out.println(method + ": ");
//		System.out.println("\t" + assertionCount.get(method) + " assertions");
//		System.out.println("\t" + failureCount.get(method) + " failed");
//		System.out.println("\t" + errorCount.get(method) + " errors");
//	}
//	System.out.println("***** FAILURES ****** (" + failures.size() + " out of " + tests + ")");
//	int i = 1;
//	for (String test: failures.keySet()) {
//		String prettyTest = test.replaceAll("execution\\(", "").replaceAll("\\)$", "");
//		String prettyMethod = failures.get(test);
//		prettyMethod = prettyMethod.replaceAll("execution\\(", "");
//		prettyMethod = prettyMethod.replaceAll("\\)$", "");
//		prettyMethod = prettyMethod.replaceAll("org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.", "");
//		prettyMethod = prettyMethod.substring(prettyMethod.indexOf(" ") + 1);
//		System.out.println(i + " " + prettyTest + ": " + prettyMethod);
//		i++;
//	}
//	System.out.println("***** ERRORS ****** (" + errors.size() + " out of " + tests + ")");
//	// TODO refactor...
//	i = 1;
//	for (String test: errors.keySet()) {
//		String prettyTest = test.replaceAll("execution\\(", "").replaceAll("\\)$", "");
//		String prettyMethod = errors.get(test);
//		prettyMethod = prettyMethod.replaceAll("execution\\(", "");
//		prettyMethod = prettyMethod.replaceAll("\\)$", "");
//		prettyMethod = prettyMethod.replaceAll("org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.", "");
//		prettyMethod = prettyMethod.substring(prettyMethod.indexOf(" ") + 1);
//		System.out.println(i + " " + prettyTest + ": " + prettyMethod + " ("
//				+ exceptions.get(test).getClass() + ")");
//		i++;
//	}
	
}
