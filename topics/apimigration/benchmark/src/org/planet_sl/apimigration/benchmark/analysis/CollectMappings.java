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
import org.planet_sl.apimigration.benchmark.anno.Unresolved;
import org.planet_sl.apimigration.benchmark.anno.Wrapping;
import org.planet_sl.apimigration.benchmark.anno.Unresolved.XML;



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
				"nu.xom",
				"\\delegateLevel{1}",
				"\\delegateLevel{2}",
				"\\delegateLevel{3}",
				"\\delegateLevel{4}",
//				"\\complianceLevel{1}", // missing/don't care/todo
//				"\\complianceLevel{2}", // need's work: critical test fail
//				"\\complianceLevel{3}" // compliant: no critical tests fail
				// TODO: level 4 fully compliant
		};
		
		writer.write("\\begin{tabular}{|l|r|r|r|r|}\\hline\n");
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
		int cloneTotal = 0;
		int compliantTotal = 0;
		int nonCompliantTotal = 0;
		int borderlineCompliantTotal = 0;
		int dontCareTotal = 0;
		int brickwalledTotal = 0;
		
		for (String from: froms) {
			
			
			simpleDelegateTotal += simpleDelegateCount.get(from);
			advancedDelegateTotal += advancedDelegateCount.get(from);
			macroTotal += macroCount.get(from);
			cloneTotal += cloneCount.get(from);
			compliantTotal += compliantCount.get(from);
			nonCompliantTotal += nonCompliantCount.get(from);
			borderlineCompliantTotal += borderlineCompliantCount.get(from);
			dontCareTotal += dontCareCount.get(from);
			brickwalledTotal += brickwalledCount.get(from);
			
			writer.write(from + " & ");
			writer.write(simpleDelegateCount.get(from) + " & ");
			writer.write(advancedDelegateCount.get(from) + " & ");
			writer.write(macroCount.get(from) + " & ");
			writer.write(cloneCount.get(from) + "" );
//			writer.write((dontCareCount.get(from) + brickwalledCount.get(from)) + " & ");
//			writer.write(nonCompliantCount.get(from) + " & ");
//			writer.write(compliantCount.get(from).toString());
			writer.write("\\\\\\hline\n");
		}
		
		writer.write("\\hline\n");
		writer.write(" & " + simpleDelegateTotal + " & ");
		writer.write(advancedDelegateTotal + " & ");
		writer.write(macroTotal + " & ");
		writer.write(cloneTotal + "\\\\\\hline");
//		writer.write((dontCareTotal + brickwalledTotal) + " & ");
//		writer.write(nonCompliantTotal + " & ");
//		writer.write(compliantTotal + "\\\\\\hline ");
		
		writer.write("\\end{tabular}\n");
		writer.flush();
		writer.close();
	}
	
	public static void printMappingTable(File file) throws IOException {
		List<String> froms = new ArrayList<String>(typeMapping.keySet());
		Collections.sort(froms);
		FileWriter writer = new FileWriter(file);	
		
		writer.write(Row.q("nu.xom") +", " + Row.q("org.jdom") + ", " + Row.q("#Mapped") + ", " + Row.q("#Derived") + "\n");
		for (String from: froms) {
		
			writer.write(Row.q(from) + ", ");
			writer.write(Row.q(typeMapping.get(from).replaceAll("org.jdom.", "")) + ", ");
			writer.write(Row.q(mappedFeatureCount.get(from)) + ", ");
			writer.write(Row.q(derivedFeatureCount.get(from)) + "\n");

			
			//System.out.print(Row.q(from) + ", ");
			//System.out.print(Row.q(typeMapping.get(from).replaceAll("org.jdom.", "")) + ", ");
//			System.out.print(Row.q(mappedFeatureCount.get(from)) + ", ");
//			System.out.println(Row.q(derivedFeatureCount.get(from)));

		
		}
		writer.flush();
		writer.close();
	}
	
	public static void printMappingTableTeX(File file) throws IOException {
		List<String> froms = new ArrayList<String>(typeMapping.keySet());
		Collections.sort(froms);
		FileWriter writer = new FileWriter(file);	
		
		String headings[] = {
				"\\xomTypeHeading",
				"\\jdomTypeHeading",
				"\\mappedFeatureHeading",
				"\\compositeFeatureHeading"
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
			writer.write(typeMapping.get(from).replaceAll("org.jdom.", "").replaceAll(";", "; ") + " & ");
			writer.write((mappedFeatureCount.get(from) == null ? 0 : mappedFeatureCount.get(from)) + " & ");
			writer.write((derivedFeatureCount.get(from) == null ? 0 : derivedFeatureCount.get(from))  + "\\\\\\hline\n");
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
		row.fromClass = from;
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
			if (anno instanceof Unresolved) {
				row.unresolved.add((Unresolved)anno);
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
				System.out.println("Found a post: " + ((Issue.Post)anno).value()[0]);
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
	static Map<String, Integer> cloneCount = new HashMap<String, Integer>();
	
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
		init(cloneCount, from);
		
		switch (anno.value()) {
		case DELEGATE: inc(simpleDelegateCount, from); break;
		case ADVANCED_DELEGATE: inc(advancedDelegateCount, from); break;
		case MACRO: inc(macroCount, from); break;
		case CLONE: inc(cloneCount, from); break;
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
			//System.out.println(row);
		}
		writer.flush();
		writer.close();
		writeIssuesTable(rows);
		writeUnresolvedIssuesTable(rows);
		writeIssuesSamplesTable(rows);
	}

	private static void writeUnresolvedIssuesTable(List<Row> rows) throws IOException {
		Map<String, Integer> preCount = new HashMap<String, Integer>();
		Map<String, Integer> postCount = new HashMap<String, Integer>();
		Map<String, Integer> invCount = new HashMap<String, Integer>();
		Map<String, Integer> preResolvedCount = new HashMap<String, Integer>();
		Map<String, Integer> postResolvedCount = new HashMap<String, Integer>();
		Map<String, Integer> invResolvedCount = new HashMap<String, Integer>();
		Map<String, Integer> preUnresolvedCount = new HashMap<String, Integer>();
		Map<String, Integer> postUnresolvedCount = new HashMap<String, Integer>();
		Map<String, Integer> invUnresolvedCount = new HashMap<String, Integer>();
		Map<String, Map<XML , Integer>> unresolveds = new HashMap<String, Map<XML, Integer>>();
		int preUnresolvedTotal = 0;
		int postUnresolvedTotal = 0;
		int invUnresolvedTotal = 0;
		int throwsUnresolvedTotal = 0;
		for (Row row: rows) {
			String type = row.fromClass;
			init(preCount, type);
			init(preResolvedCount, type);
			init(preUnresolvedCount, type);
			init(postCount, type);
			init(postResolvedCount, type);
			init(postUnresolvedCount, type);
			init(invCount, type);
			init(invResolvedCount, type);
			init(invUnresolvedCount, type);
			
			if (!unresolveds.containsKey(type)) {
				unresolveds.put(type, new HashMap<XML,Integer>());
			}
			
			
			for (Unresolved unr: row.unresolved) {
				if (!unresolveds.get(type).containsKey(unr.value()[0])) {
					unresolveds.get(type).put(unr.value()[0], 0);
				}
				unresolveds.get(type).put(unr.value()[0], unresolveds.get(type).get(unr.value()[0]) + 1);
			}
				
			
			if (row.preIssue != null) {
				inc(preCount, type);
				if (row.preIssue.resolved()) {
					inc(preResolvedCount, type);
				}
				else {
					inc(preUnresolvedCount, type);
					preUnresolvedTotal++;
				}
			}
			if (row.postIssue != null) {
				inc(postCount, type);
				if (row.postIssue.resolved()) {
					inc(postResolvedCount, type);
				}
				else {
					inc(postUnresolvedCount, type);
					postUnresolvedTotal++;
				}
			}
			if (row.invariantIssue != null) {
				inc(invCount, type);
				if (row.invariantIssue.resolved()) {
					inc(invResolvedCount, type);
				}
				else {
					inc(invUnresolvedCount, type);
					invUnresolvedTotal++;
				}
			}
		}
		List<String> types = new ArrayList<String>(preCount.keySet());
		Collections.sort(types);
		FileWriter writer = new FileWriter(new File("unrissues.tex"));
		writer.write("\\begin{tabular}{|l|r|r||r|r|r|r|r|r|}\\hline\n");
		writer.write("\\typeHeadingIssues & \\preHeading &\\postHeading &");
		
		writer.write("\\BaseURIHeading & ");
		writer.write("\\EscapingHeading & ");
		writer.write("\\NamespacingHeading & ");
		writer.write("\\SerializationHeading & ");
		writer.write("\\ParsingHeading & ");
		writer.write("\\DocTypeValidityHeading ");

		
		writer.write("\\\\\\hline\\hline\n");
		int baseuriTotal = 0;
		int escapingTotal = 0;
		int namespacingTotal = 0;
		int serializationTotal = 0;
		int parsingTotal = 0;
		int doctypeTotal = 0;
		
		
		for (String type: types) {
			writer.write(type + " & " + (preUnresolvedCount.get(type) == 0 ? "\\ZERO" : preUnresolvedCount.get(type)));
			writer.write(" & " + (postUnresolvedCount.get(type) == 0 ? "\\ZERO" : postUnresolvedCount.get(type)));
//			writer.write(" & " + invUnresolvedCount.get(type));
			
			Integer baseuri = unresolveds.get(type).get(XML.BaseURI);
			if (baseuri == null) baseuri = 0;
			Integer escaping = unresolveds.get(type).get(XML.Escaping);
			if (escaping == null) escaping = 0;
			Integer namespacing = unresolveds.get(type).get(XML.Namespacing);
			if (namespacing == null) namespacing = 0;
			Integer serialization = unresolveds.get(type).get(XML.Serialization);
			if (serialization == null) serialization = 0;
			Integer parsing = unresolveds.get(type).get(XML.Parsing);
			if (parsing == null) parsing = 0;
			Integer doctype = unresolveds.get(type).get(XML.DocTypeValidity);
			if (doctype == null) doctype = 0;
			
			baseuriTotal += baseuri;
			escapingTotal += escaping;
			namespacingTotal += namespacing;
			serializationTotal += serialization;
			//Temporarily!! Ugly HACK!
			//doctypeTotal += parsing;
			parsingTotal += parsing;
			doctypeTotal += doctype;
			
			writer.write(" & " +  (baseuri == 0 ? "\\ZERO" : baseuri));
			writer.write(" & " +  (escaping == 0 ? "\\ZERO" : escaping));
			writer.write(" & " +  (namespacing == 0 ? "\\ZERO" : namespacing));
			writer.write(" & " +  (serialization == 0 ? "\\ZERO" : serialization));
			writer.write(" & " +  (parsing == 0 ? "\\ZERO" : parsing));
//			writer.write(" & " +  ((doctype +parsing) == 0 ? "\\ZERO" : (doctype + parsing)));
			writer.write(" & " +  (doctype == 0 ? "\\ZERO" : doctype));
			
			writer.write("\\\\\\hline\n");
		}
		writer.write("\\hline\n");
		writer.write(" & " + preUnresolvedTotal);
		writer.write(" & " + postUnresolvedTotal);
		
		writer.write(" & " + baseuriTotal);
		writer.write(" & " + escapingTotal);
		writer.write(" & " + namespacingTotal);
		writer.write(" & " + serializationTotal);
		writer.write(" & " + parsingTotal);
		writer.write(" & " + doctypeTotal + "\\\\\\hline");
		
		writer.write("\\end{tabular}\n");
		writer.close();
	}

	
	private static void writeIssuesTable(List<Row> rows) throws IOException {
		Map<String, Integer> preCount = new HashMap<String, Integer>();
		Map<String, Integer> postCount = new HashMap<String, Integer>();
		Map<String, Integer> invCount = new HashMap<String, Integer>();
		Map<String, Integer> throwsCount = new HashMap<String, Integer>();
		Map<String, Integer> preResolvedCount = new HashMap<String, Integer>();
		Map<String, Integer> postResolvedCount = new HashMap<String, Integer>();
		Map<String, Integer> invResolvedCount = new HashMap<String, Integer>();
		Map<String, Integer> throwsResolvedCount = new HashMap<String, Integer>();
		Map<String, Integer> preUnresolvedCount = new HashMap<String, Integer>();
		Map<String, Integer> postUnresolvedCount = new HashMap<String, Integer>();
		Map<String, Integer> invUnresolvedCount = new HashMap<String, Integer>();
		Map<String, Integer> throwsUnresolvedCount = new HashMap<String, Integer>();
		int preResolvedTotal = 0;
		int postResolvedTotal = 0;
		int invResolvedTotal = 0;
		int throwsResolvedTotal = 0;
		for (Row row: rows) {
			String type = row.fromClass;
			init(preCount, type);
			init(preResolvedCount, type);
			init(preUnresolvedCount, type);
			init(postCount, type);
			init(postResolvedCount, type);
			init(postUnresolvedCount, type);
			init(invCount, type);
			init(invResolvedCount, type);
			init(invUnresolvedCount, type);
			init(throwsCount, type);
			init(throwsResolvedCount, type);
			init(throwsUnresolvedCount, type);
			
			if (row.preIssue != null) {
				inc(preCount, type);
				if (row.preIssue.resolved()) {
					inc(preResolvedCount, type);
					preResolvedTotal++;
				}
				else {
					inc(preUnresolvedCount, type);
				}
			}
			if (row.postIssue != null) {
				inc(postCount, type);
				if (row.postIssue.resolved()) {
					inc(postResolvedCount, type);
					postResolvedTotal++;
				}
				else {
					inc(postUnresolvedCount, type);
				}
			}
			if (row.invariantIssue != null) {
				inc(invCount, type);
				if (row.invariantIssue.resolved()) {
					inc(invResolvedCount, type);
					invResolvedTotal++;
				}
				else {
					inc(invUnresolvedCount, type);
				}
			}
			if (row.throwsIssue != null) {
				inc(throwsCount, type);
				if (row.throwsIssue.resolved()) {
					inc(throwsResolvedCount, type);
					throwsResolvedTotal++;
				}
				else {
					inc(throwsUnresolvedCount, type);
				}
			}
		}
		List<String> types = new ArrayList<String>(preCount.keySet());
		Collections.sort(types);
		FileWriter writer = new FileWriter(new File("issues.tex"));
		writer.write("\\begin{tabular}{|l|r|r|r|r|}\\hline\n");
		writer.write("\\typeHeadingIssues & \\preHeading &\\postHeading & \\invariantHeading & \\throwsHeading\\\\\\hline\\hline\n");
		for (String type: types) {
			writer.write(type + " & " + (preResolvedCount.get(type) == 0 ? "\\ZERO" : preResolvedCount.get(type)));
			writer.write(" & " + (postResolvedCount.get(type) == 0 ? "\\ZERO" : postResolvedCount.get(type)));
			writer.write(" & " + (invResolvedCount.get(type)  == 0 ? "\\ZERO" : invResolvedCount.get(type)));
			writer.write(" & " + (throwsResolvedCount.get(type) == 0 ? "\\ZERO" : throwsResolvedCount.get(type)));
			writer.write("\\\\\\hline\n");
		}
		writer.write("\\hline\n");
		writer.write(" & " + preResolvedTotal);
		writer.write(" & " + postResolvedTotal);
		writer.write(" & " + invResolvedTotal);
		writer.write(" & " + throwsResolvedTotal + "\\\\\\hline");
		writer.write("\\end{tabular}\n");
		writer.close();
	}
	
	private static String simpleFeature(AccessibleObject feature) {
		String str = feature.toString();
		//str = str.substring(str.indexOf("_xom") + 4);
		str = str.replaceAll("org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom.", "");
		str = str.replaceAll("java.lang.", "");
		str = str.substring(str.indexOf(".") + 1);
		str = str.replaceAll("^public ", "");
		return str.replaceAll("\\$", "\\\\\\$");
	}
	
	private static void writeIssuesSamplesTable(List<Row> rows) throws IOException {
		List<String> types = new ArrayList<String>();
		Map<String,List<Row>> typeRows = new HashMap<String, List<Row>>();
		for (Row row: rows) {
			String type = row.fromClass;
			if (!types.contains(type)) {
				types.add(type);
			}
			if (!typeRows.containsKey(type)) {
				typeRows.put(type, new ArrayList<Row>());
			}
			if (type.equals("Attribute")) {
			System.out.println("Adding row for type: " + row);
			System.out.println(row.postIssue);
			}
			
			typeRows.get(type).add(row);
		}
		Collections.sort(types);
		
		
		FileWriter writer = new FileWriter(new File("samples.tex"));
		writer.write("\\begin{tabular}{|l|l|l|p{0.4\\textwidth}|}\\hline\n");
		writer.write("\\typeHeadingSamples & \\featureHeadingSamples &\\categoryHeadingSamples & \\commentHeadingSamples\\\\\\hline\\hline\n");
		for (String type: types) {
			String str = type;
			if (!typeRows.get(type).isEmpty()) {
				for (Row row: typeRows.get(type)) {
					if (type.equals("Attribute")) {
						System.out.println("ATTRIBUTE's post: " + row.postIssue);
					}
					if (row.preIssue != null && row.preIssue.resolved()) {
						if (simpleFeature(row.from).equals("Attribute(String,String)")) {
							writer.write(str + " & ");
							writer.write(simpleFeature(row.from) + " & ");
							writer.write("Pre & ");
							writer.write(row.preIssue.value()[0].replaceAll("&", "\\&"));
							writer.write("\\\\\\hline\n");
							str = "";
						}
					}
					if (row.postIssue != null && row.postIssue.resolved()) {
						System.out.println("POST for " + type + ": " + simpleFeature(row.from));
						if (simpleFeature(row.from).indexOf("toXML") > - 1) {
							writer.write(str + " & ");
							writer.write(simpleFeature(row.from) + " & ");
							writer.write("Post & ");
							writer.write(row.postIssue.value()[0].replaceAll("&", "\\&"));
							writer.write("\\\\\\hline\n");
							str = "";
						}
					}
					if (row.invariantIssue != null && row.invariantIssue.resolved()) {
						if (simpleFeature(row.from).equals("setSystemID(String)")) {
							writer.write(str + " & ");
							writer.write(simpleFeature(row.from) + " & ");
							writer.write("Invariant & ");
							writer.write(row.invariantIssue.value()[0].replaceAll("&", "\\&"));
							writer.write("\\\\\\hline\n");
							str = "";
						}
					}
					if (row.throwsIssue != null && row.throwsIssue.resolved()) {
						if (simpleFeature(row.from).equals("addAttribute(Attribute)")) {
							writer.write(str + " & ");
							writer.write(simpleFeature(row.from) + " & ");
							writer.write("Throws & ");
							writer.write(row.throwsIssue.value()[0].replaceAll("&", "\\&"));
							writer.write("\\\\\\hline\n");
							str = "";
						}
					}
					
				}
			}
		}
		writer.write("\\end{tabular}\n");
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
