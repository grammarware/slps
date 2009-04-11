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
	static Map<String, String> failures = new HashMap<String, String>();
	static Map<String, String> errors = new HashMap<String, String>();
	static Map<String, String> success = new HashMap<String, String>();
	static Map<String, RuntimeException> exceptions = new HashMap<String, RuntimeException>();
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
		System.out.println(tests + " tests with " + assertions + " assertions");
		Collections.sort(methods);
		for (String method: methods) {
			System.out.println(method + ": ");
			System.out.println("\t" + assertionCount.get(method) + " assertions");
			System.out.println("\t" + failureCount.get(method) + " failed");
			System.out.println("\t" + errorCount.get(method) + " errors");
		}
//		System.out.println("***** FAILURES ****** (" + failures.size() + " out of " + tests + ")");
//		int i = 1;
//		for (String test: failures.keySet()) {
//			String prettyTest = test.replaceAll("execution\\(", "").replaceAll("\\)$", "");
//			String prettyMethod = failures.get(test);
//			prettyMethod = prettyMethod.replaceAll("execution\\(", "");
//			prettyMethod = prettyMethod.replaceAll("\\)$", "");
//			prettyMethod = prettyMethod.replaceAll("org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.", "");
//			prettyMethod = prettyMethod.substring(prettyMethod.indexOf(" ") + 1);
//			System.out.println(i + " " + prettyTest + ": " + prettyMethod);
//			i++;
//		}
//		System.out.println("***** ERRORS ****** (" + errors.size() + " out of " + tests + ")");
//		// TODO refactor...
//		i = 1;
//		for (String test: errors.keySet()) {
//			String prettyTest = test.replaceAll("execution\\(", "").replaceAll("\\)$", "");
//			String prettyMethod = errors.get(test);
//			prettyMethod = prettyMethod.replaceAll("execution\\(", "");
//			prettyMethod = prettyMethod.replaceAll("\\)$", "");
//			prettyMethod = prettyMethod.replaceAll("org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.", "");
//			prettyMethod = prettyMethod.substring(prettyMethod.indexOf(" ") + 1);
//			System.out.println(i + " " + prettyTest + ": " + prettyMethod + " ("
//					+ exceptions.get(test).getClass() + ")");
//			i++;
//		}
	}
	
	private static Map<String,Integer> failureCount = new HashMap<String, Integer>();
	private static Map<String,Integer> errorCount = new HashMap<String, Integer>();
	private static Map<String,Integer> assertionCount = new HashMap<String, Integer>();
	private static java.util.List<String> methods = new ArrayList<String>();
	private static int assertions = 0;
	private static String testedMethod = "<unknown>";
	private static void inc(Map<String,Integer> table, String currentMethod) {
		if (!table.containsKey(currentMethod)) {
			table.put(currentMethod, 0);
		}
		table.put(currentMethod, table.get(currentMethod) + 1);
	}
	
	before() : call(void assert*(..)) {
		inc(assertionCount, testedMethod);
		assertions++;
	}
	
	before (): execution(void test*()) {
		currentTest = thisJoinPoint.toShortString();
		testedMethod = "<unknown>";
	}
	
	void around(): execution(void test*()) {
		tests++;
		try {
			proceed();
		}
		catch (AssertionError e) {
			failures.put(currentTest, testedMethod);
			inc(failureCount, testedMethod);
			throw e;
		}
		catch (RuntimeException e) {
			errors.put(currentTest, testedMethod);
			exceptions.put(currentTest, e);
			inc(errorCount, testedMethod);
			throw e;
		}
		// We cannot count success; because than we only count it for the method
		// in the last assertion.
//		success.put(currentTest, currentMethod);
	}
	
	before(): execution(* org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.*.*(..)) {
		currentMethod = thisJoinPoint.toString();
		String prettyName = prettyName(currentMethod);
		String name = prettyName.substring(prettyName.indexOf(".") + 1);
		name = name.replaceAll("\\(.*\\)$", "");
		name = name.toLowerCase();
		if (currentTest.toLowerCase().indexOf(name) != -1) {
			testedMethod = prettyName;
		}
		if (!methods.contains(testedMethod)) {
			methods.add(testedMethod);
		}
	}
	
	
}
