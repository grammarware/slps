package org.planet_sl.apimigration.benchmark.analysis;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.planet_sl.apimigration.benchmark.anno.Issue;
import org.planet_sl.apimigration.benchmark.anno.MapsTo;
import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Wrapping;
import org.planet_sl.apimigration.benchmark.anno.Issue.Doc;
import org.planet_sl.apimigration.benchmark.anno.Issue.Invariant;
import org.planet_sl.apimigration.benchmark.anno.Issue.Post;
import org.planet_sl.apimigration.benchmark.anno.Issue.Pre;
import org.planet_sl.apimigration.benchmark.anno.Issue.Throws;



public class CollectMappings {

	private static final String PKG = "org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom";
	
	private static final String XOM_CLASSES[] = {"Attribute",
			"Builder",
//			"XPathContext",
			"Comment",
			"DocType",
			"Document",
			"Element",
			"Elements",
//			"IllegalAddException",
//			"IllegalCharacterDataException",
//			"IllegalDataException",
//			"IllegalNameException",
//			"IllegalTargetException",
//			"LocatorFilter",
//			"MalformedURIException",
//			"MultipleParentException",
//			"ValidityException",
//			"WellformednessException",
//			"XMLException",
//			"XPathException",
//			"Namespace",
//			"NamespaceConflictException",
			"Node",
//			"NodeFactory",
			"Nodes",
//			"NoSuchAttributeException",
//			"NoSuchChildException",
			"ParentNode",
//			"ParsingException",
			"ProcessingInstruction",
//			"Serializer",
			"Text" 
//			"Utils"
			};
	
	public static void main(String arg[]) throws ClassNotFoundException, IOException {
		//		classMappings();
		methodMappings(new File("annos.csv"));
		printMappingTable(new File("mappings.csv"));
		printImplMappingTable(new File("implmapping.tex"));
		printMappingTableTeX(new File("dommapping.tex"));
		
	}
	
	public static void printImplMappingTable(File file) throws IOException {
		List<String> froms = new ArrayList<String>(typeMapping.keySet());
		Collections.sort(froms);
		FileWriter writer = new FileWriter(file);	
	
		String headings[] = {
				"XOM Type",
				"SD",
				"AD",
				"M",
				"C",
				"NC",
				"BC",
				"DC",
				"HBW"
		};
		
		writer.write("\\begin{tabular}{|l|r|r|r|r|r|r|r|r|}\\hline\n");
		for (int i = 0; i < headings.length; i++) {
			writer.write(headings[i]);
			if (i < headings.length - 1) {
				writer.write(" & ");
			}
			else {
				writer.write("\\\\\\hline\\hline\n");
			}
			
		}
		
		
		int simpleDelegateTotal = 0;
		int advancedDelegateTotal = 0;
		int macroTotal = 0;
		int compliantTotal = 0;
		int nonCompliantTotal = 0;
		int borderlineCompliantTotal = 0;
		int dontCareTotal = 0;
		int brickwalledTotal = 0;
		
		for (String from: froms) {
			
			
			simpleDelegateTotal += simpleDelegateCount.get(from);
			advancedDelegateTotal += advancedDelegateCount.get(from);
			macroTotal += macroCount.get(from);
			compliantTotal += compliantCount.get(from);
			nonCompliantTotal += nonCompliantCount.get(from);
			borderlineCompliantTotal += borderlineCompliantCount.get(from);
			dontCareTotal += dontCareCount.get(from);
			brickwalledTotal += brickwalledCount.get(from);
			
			writer.write(from + " & ");
			writer.write(simpleDelegateCount.get(from) + " & ");
			writer.write(advancedDelegateCount.get(from) + " & ");
			writer.write(macroCount.get(from) + " & ");
			writer.write(compliantCount.get(from) + " & ");
			writer.write(nonCompliantCount.get(from) + " & ");
			writer.write(borderlineCompliantCount.get(from) + " & ");
			writer.write(dontCareCount.get(from) + " & ");
			writer.write(brickwalledCount.get(from).toString());
			writer.write("\\\\\\hline\n");
		}
		
		writer.write("\\hline\n");
		writer.write(" & " + simpleDelegateTotal + " & ");
		writer.write(advancedDelegateTotal + " & ");
		writer.write(macroTotal + " & ");
		writer.write(compliantTotal + " & ");
		writer.write(nonCompliantTotal + " & ");
		writer.write(borderlineCompliantTotal + " & ");
		writer.write(dontCareTotal + " & ");
		writer.write(brickwalledTotal + "\\\\\\hline\n");
		
		writer.write("\\end{tabular}\n");
		writer.flush();
		writer.close();
	}
	
	public static void printMappingTable(File file) throws IOException {
		List<String> froms = new ArrayList<String>(typeMapping.keySet());
		Collections.sort(froms);
		FileWriter writer = new FileWriter(file);	
		
		writer.write(Row.q("XOM type") +", " + Row.q("JDOM type") + ", " + Row.q("#Mapped") + ", " + Row.q("#Derived") + "\n");
		for (String from: froms) {
		
			writer.write(Row.q(from) + ", ");
			writer.write(Row.q(typeMapping.get(from).replaceAll("org.jdom.", "")) + ", ");
			writer.write(Row.q(mappedFeatureCount.get(from)) + ", ");
			writer.write(Row.q(derivedFeatureCount.get(from)) + "\n");

			
			System.out.print(Row.q(from) + ", ");
			System.out.print(Row.q(typeMapping.get(from).replaceAll("org.jdom.", "")) + ", ");
			System.out.print(Row.q(mappedFeatureCount.get(from)) + ", ");
			System.out.println(Row.q(derivedFeatureCount.get(from)));

		
		}
		writer.flush();
		writer.close();
	}
	
	public static void printMappingTableTeX(File file) throws IOException {
		List<String> froms = new ArrayList<String>(typeMapping.keySet());
		Collections.sort(froms);
		FileWriter writer = new FileWriter(file);	
		
		String headings[] = {
				"XOM Type",
				"JDOM Type",
				"\\#Mapped",
				"\\#Derived"
		};

		writer.write("\\begin{tabular}{|l|l|r|r|}\\hline\n");
		for (int i = 0; i < headings.length; i++) {
			writer.write(headings[i]);
			if (i < headings.length - 1) {
				writer.write(" & ");
			}
			else {
				writer.write("\\\\\\hline\\hline\n");
			}
			
		}
		
		int mappedTotal = 0;
		int derivedTotal = 0;
		
		for (String from: froms) {
			mappedTotal += mappedFeatureCount.get(from) == null ? 0 : mappedFeatureCount.get(from);
			derivedTotal += derivedFeatureCount.get(from) == null ? 0 : derivedFeatureCount.get(from);
			writer.write(from + " & ");
			writer.write(typeMapping.get(from).replaceAll("org.jdom.", "") + " & ");
			writer.write(mappedFeatureCount.get(from) + " & ");
			writer.write(derivedFeatureCount.get(from) + "\\\\\\hline\n");
		}
		
		writer.write("\\hline\n");
		
		writer.write(" & & " + mappedTotal + " & " + derivedTotal + "\\\\\\hline\n");
		writer.write("\\end{tabular}");
		writer.flush();
		writer.close();
	}
	
	public static void classMappings() throws ClassNotFoundException {
		for (String s: XOM_CLASSES) {
			Class<?> klass = Class.forName(PKG + "." + s);
			Annotation[] annos = klass.getAnnotations();
			for (Annotation anno: annos) {
				if (anno.annotationType().equals(MapsTo.class)) {
					System.out.println("\"nu.xom." + s + "\", \"" + ((MapsTo)anno).value() + "\"");
				}
			}
		}
	}

	
	
	static Map<String,String> typeMapping = new HashMap<String,String>();
	static Map<String,Integer> mappedFeatureCount = new HashMap<String,Integer>();
	static Map<String,Integer> derivedFeatureCount = new HashMap<String,Integer>();
	
	
	public static void addRow(String from, String to, List<Row> rows, AccessibleObject o) {
		Row row = new Row();
		row.from = o;
		boolean mapped = false;
		for (Annotation anno: o.getAnnotations()) {
			if (anno instanceof Wrapping) {
				// should not be in table
				return;
			}
			if (anno instanceof MapsTo) {
				mapped = true;
				row.to = (MapsTo)anno;
			}
			if (anno instanceof Progress) {
				row.progress = (Progress)anno;
				incProgress(from, (Progress)anno);
			}
			if (anno instanceof Solution) {
				row.solution = (Solution)anno;
				incSolution(from, (Solution)anno);
			}
			if (anno instanceof Issue.Pre) {
				row.preIssue = (Issue.Pre)anno;
			}
			if (anno instanceof Issue.Post) {
				row.postIssue = (Issue.Post)anno;
			}
			if (anno instanceof Issue.Throws) {
				row.throwsIssue = (Issue.Throws)anno;
			}
			if (anno instanceof Issue.Invariant) {
				row.invariantIssue = (Issue.Invariant)anno;
			}
			if (anno instanceof Issue.Doc) {
				row.docIssue = (Issue.Doc)anno;
			}
		}
		if (mapped) {
			if (!mappedFeatureCount.containsKey(from)) {
				mappedFeatureCount.put(from, 0);
			}
			if (!derivedFeatureCount.containsKey(from)) {
				derivedFeatureCount.put(from, 0);
			}
			if (!row.to.value().equals("")) {
				mappedFeatureCount.put(from, mappedFeatureCount.get(from) + 1);
			}
			else {
				derivedFeatureCount.put(from, derivedFeatureCount.get(from) + 1);
			}
			rows.add(row);
		}
	}
	
	
	static Map<String, Integer> simpleDelegateCount = new HashMap<String, Integer>();
	static Map<String, Integer> advancedDelegateCount = new HashMap<String, Integer>();
	static Map<String, Integer> macroCount = new HashMap<String, Integer>();
	
	static Map<String, Integer> compliantCount = new HashMap<String, Integer>();
	static Map<String, Integer> nonCompliantCount = new HashMap<String, Integer>();
	static Map<String, Integer> borderlineCompliantCount = new HashMap<String, Integer>();
	static Map<String, Integer> dontCareCount = new HashMap<String, Integer>();
	static Map<String, Integer> brickwalledCount = new HashMap<String, Integer>();
	
	
	
	private static void init(Map<String,Integer> map, String key) {
		if (!map.containsKey(key)) {
			map.put(key, 0);
		}
	}
	
	private static void inc(Map<String,Integer> map, String key) {
		map.put(key, map.get(key) + 1);
	}
	
	private static void incSolution(String from, Solution anno) {
		init(simpleDelegateCount, from);
		init(advancedDelegateCount, from);
		init(macroCount, from);
		
		switch (anno.value()) {
		case DELEGATE: inc(simpleDelegateCount, from); break;
		case ADVANCED_DELEGATE: inc(advancedDelegateCount, from); break;
		case MACRO: inc(macroCount, from); break;
		}
	}

	private static void incProgress(String from, Progress anno) {
		init(compliantCount, from);
		init(nonCompliantCount, from);
		init(borderlineCompliantCount, from);
		init(dontCareCount, from);
		init(brickwalledCount, from);
		
		switch (anno.value()) {
		case OK : inc(compliantCount, from); break;
		case NEEDSWORK : inc(nonCompliantCount, from); break;
		case DONTCARE: inc(dontCareCount, from); break;
		case GIVENUP: inc(brickwalledCount, from); break;
		}
	}

	public static void methodMappings(File file) throws ClassNotFoundException, IOException {
		List<Row> rows = new ArrayList<Row>();
		for (String s: XOM_CLASSES) {
			Class<?> klass = Class.forName(PKG + "." + s);
			MapsTo map = klass.getAnnotation(MapsTo.class);
			String from = klass.getSimpleName();
			String to = map == null ? "" : map.value();
			if (map != null) {
				if (!map.equals("")) {
					typeMapping.put(from, to);
				}
			}
			for (Method method: klass.getDeclaredMethods()) {
				addRow(from, to, rows, method);
			}
			for (Constructor<?> cons: klass.getDeclaredConstructors()) {
				addRow(from, to, rows, cons);
			}
		}
		FileWriter writer = new FileWriter(file);
		writer.write(Row.HEADER + "\n");
		for (Row row: rows) {
			writer.write(row.toString() + "\n");
			System.out.println(row);
		}
		writer.flush();
		writer.close();
	}

	public static String paramTypesToString(Class<?> classes[]) {
		StringBuilder s = new StringBuilder();
		for (int i = 0; i < classes.length; i++) {
			s.append(classes[i].getSimpleName());
			if (i < classes.length - 1) {
				s.append(", ");
			}
		}
		return s.toString();
	}
}
