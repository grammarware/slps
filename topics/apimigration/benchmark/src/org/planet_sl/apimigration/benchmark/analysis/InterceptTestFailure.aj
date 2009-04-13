package org.planet_sl.apimigration.benchmark.analysis;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
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
	
	public static Map<String,Integer> failOnly = new HashMap<String, Integer>();
	public static Map<String,Integer> errorOnly = new HashMap<String, Integer>();
	public static Map<String,Integer> successOnly = new HashMap<String, Integer>();
	public static Map<String,Integer> mixedOnly = new HashMap<String, Integer>();
	public static Map<String,Integer> nonSuccessOnly = new HashMap<String, Integer>();
	
	private static String typeName(String string) {
		string = string.replaceAll("execution\\(", "");
		string = string.substring(0, string.indexOf("."));
		return string;
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
		int c = 0;
		for (String method: successCount.keySet()) {
			if (!failureCount.containsKey(method) && !errorCount.containsKey(method)) {
				c++;
				inc(successOnly, typeName(method));
				System.out.println("only successes for " + method + ": " + successCount.get(method));
			}
		}
		System.out.println("Total only-success features: " + c);
		c = 0;
		for (String method: failureCount.keySet()) {
			if (!successCount.containsKey(method) && !errorCount.containsKey(method)) {
				c++;
				inc(failOnly, typeName(method));
				inc(nonSuccessOnly, typeName(method));
				System.out.println("only failures for " + method + ": " + failureCount.get(method));
			}
		}
		System.out.println("Total only-failure features: " + c);
		c = 0;
		for (String method: errorCount.keySet()) {
			if (!successCount.containsKey(method) && !failureCount.containsKey(method)) {
				c++;
				inc(errorOnly, typeName(method));
				inc(nonSuccessOnly, typeName(method));
				System.out.println("only errors for " + method + ": " + errorCount.get(method));
			}
		}
		System.out.println("Total only-error features: " + c);
		
		Set<String> mixedMethods = new HashSet<String>();
		mixedMethods.addAll(errorCount.keySet());
		mixedMethods.addAll(failureCount.keySet());
		mixedMethods.addAll(successCount.keySet());
		
		c = 0;
		for (String method: mixedMethods) {
			if ((successCount.containsKey(method) && failureCount.containsKey(method)) ||
					(successCount.containsKey(method) && errorCount.containsKey(method))) {
				c++;
				inc(mixedOnly, typeName(method));
			}
		}
		System.out.println("Number of mixed result features: " + c);
		
		System.out.println("Number of mixed result per type");
		List<String> mixedOnlyTypes = new ArrayList<String>(mixedOnly.keySet());
		Collections.sort(mixedOnlyTypes);
		
		for (String type: mixedOnlyTypes) {
			System.out.println(type + ": " + mixedOnly.get(type));
		}
		
		System.out.println("Number of succes only per type");
		List<String> successOnlyTypes = new ArrayList<String>(successOnly.keySet());
		Collections.sort(successOnlyTypes);
		for (String type: successOnlyTypes) {
			System.out.println(type + ": " + successOnly.get(type));
		}
		
		System.out.println("\nNumber of non-success only per type");
		List<String> nonSuccessOnlyTypes = new ArrayList<String>(nonSuccessOnly.keySet());
		Collections.sort(nonSuccessOnlyTypes);
		for (String type: nonSuccessOnlyTypes) {
			System.out.println(type + ": " + nonSuccessOnly.get(type));
		}
		
		try {
			Map<String,Integer> totalSuccessPerType = countPerType(successCount);
			Map<String,Integer> totalFailurePerType = countPerType(failureCount);
			Map<String,Integer> totalErrorPerType = countPerType(errorCount);
			
			Set<String> allTypes = new HashSet<String>(mixedOnlyTypes);
			allTypes.addAll(successOnlyTypes);
			allTypes.addAll(nonSuccessOnlyTypes);
			List<String> allTypesList = new ArrayList<String>(allTypes);
			Collections.sort(allTypesList);
			for (String type: allTypesList) {
				if (!mixedOnly.containsKey(type)) {
					mixedOnly.put(type, 0);
				}
				if (!successOnly.containsKey(type)) {
					successOnly.put(type, 0);
				}
				if (!nonSuccessOnly.containsKey(type)) {
					nonSuccessOnly.put(type, 0);
				}
				if (!totalSuccessPerType.containsKey(type)) {
					totalSuccessPerType.put(type, 0);
				}
				if (!totalFailurePerType.containsKey(type)) {
					totalFailurePerType.put(type, 0);
				}
				if (!totalErrorPerType.containsKey(type)) {
					totalErrorPerType.put(type, 0);
				}
			}
			File file = new File("compliance.tex");
			FileWriter writer = new FileWriter(file);
			writer.write("\\begin{tabular}{|l|r|r|r||r|r|r|}\\hline\n");
			writer.write("\\typeHeading\t\t\t\t& ");
			writer.write("\\successHeading & ");
			writer.write("\\failureHeading &");
			writer.write("\\errorHeading &");
			writer.write("\\successOnlyHeading\t& \\nonSuccessOnlyHeading \t& \\mixedHeading");
			writer.write("\\\\\\hline\\hline\n");
			int totalFailure = 0;
			int totalError = 0;
			int totalSuccess = 0;
			int totalSuccessOnly = 0;
			int totalNonSuccessOnly = 0;
			int totalMixedOnly = 0;
			for (String type: allTypesList) {
				writer.write(type + "\t\t& ");
				
				totalSuccess += totalSuccessPerType.get(type);
				totalFailure += totalFailurePerType.get(type);
				totalError += totalErrorPerType.get(type);
				totalSuccessOnly += successOnly.get(type);
				totalNonSuccessOnly += nonSuccessOnly.get(type);
				totalMixedOnly += mixedOnly.get(type);
				
				writer.write(totalSuccessPerType.get(type) + "\t& ");
				writer.write(totalFailurePerType.get(type) + "\t& ");
				writer.write(totalErrorPerType.get(type) + "\t& ");
				
				writer.write(successOnly.get(type) + "\t& ");
				writer.write(nonSuccessOnly.get(type) + "\t& ");
				writer.write(mixedOnly.get(type) + "\\\\\\hline\n");
			}
			writer.write("\\hline\n");
			writer.write(" & " + totalSuccess);
			writer.write(" & " + totalFailure);
			writer.write(" & " + totalError);
			writer.write(" & " + totalSuccessOnly);
			writer.write(" & " + totalNonSuccessOnly);
			writer.write(" & " + totalMixedOnly + "\\\\\\hline\n");
			writer.write("\\end{tabular}\n");
			writer.flush();
			writer.close();
		}
		catch (IOException e) {
			e.printStackTrace();
		}
		
		
	}
	
	private static Map<String,Integer> countPerType(Map<String,Integer> count) {
		Map<String,Integer> perType = new HashMap<String, Integer>();
		for (String method: count.keySet()) {
			if (!perType.containsKey(typeName(method))) {
				perType.put(typeName(method), 0);
			}
			perType.put(typeName(method), perType.get(typeName(method)) + 
				(count.get(method) == null ? new Integer(0) : count.get(method)));
		}
		return perType;
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
		if (currentMethod.matches(".*Utils\\..*")) {
			return;
		}
		if (currentMethod.matches(".*Test.*")) {
			return;
		}
		if (currentMethod.matches(".*Exception.*")) {
			return;
		}
		if (currentMethod.matches(".*NodeFactory.*")) {
			return;
		}
		if (currentMethod.matches(".*Element\\.query.*")) {
			return;
		}
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
			for (StackTraceElement elt: e.getStackTrace()) {
				//
			}
			failuresTotal++;
			for (String method: methods) {
				inc(failureCount, method);
			}
			throw e;
		}
		catch (RuntimeException e) {
			errorsTotal++;
			for (String method: methods) {
				inc(errorCount, method);
			}
			throw e;
		}
		catch (Throwable e) {
			errorsTotal++;
			for (String method: methods) {
				inc(errorCount, method);
			}
			throw new RuntimeException(e);
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
