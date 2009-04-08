package org.planet_sl.apimigration.benchmark.anno;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;



public class Mappings {

	private static final String PKG = "org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom";
	
	private static final String CLASSES[] = {"Attribute",
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
	}
	
	public static void classMappings() throws ClassNotFoundException {
		for (String s: CLASSES) {
			Class<?> klass = Class.forName(PKG + "." + s);
			Annotation[] annos = klass.getAnnotations();
			for (Annotation anno: annos) {
				if (anno.annotationType().equals(MapsTo.class)) {
					System.out.println("\"nu.xom." + s + "\", \"" + ((MapsTo)anno).value() + "\"");
				}
			}
		}
	}

	public static void addRow(List<Row> rows, AccessibleObject o) {
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
			}
			if (anno instanceof Solution) {
				row.solution = (Solution)anno;
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
			rows.add(row);
		}
	}
	
	public static void methodMappings(File file) throws ClassNotFoundException, IOException {
		List<Row> rows = new ArrayList<Row>();
		for (String s: CLASSES) {
			Class<?> klass = Class.forName(PKG + "." + s);
			for (Method method: klass.getDeclaredMethods()) {
				addRow(rows, method);
			}
			for (Constructor<?> cons: klass.getDeclaredConstructors()) {
				addRow(rows, cons);
			}
		}
		FileWriter writer = new FileWriter(file);
		writer.write(Row.HEADER + "\n");
		for (Row row: rows) {
			writer.write(row.toString() + "\n");
			//System.out.println(row);
		}
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
