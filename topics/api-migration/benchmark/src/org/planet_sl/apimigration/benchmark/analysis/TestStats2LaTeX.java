package org.planet_sl.apimigration.benchmark.analysis;

import java.io.File;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jdom.*;
import org.jdom.filter.ElementFilter;
import org.jdom.input.SAXBuilder;

public class TestStats2LaTeX {

	
	public static void main(String args[]) throws JDOMException, IOException {
		SAXBuilder builder = new SAXBuilder();
		Document doc = builder.build(new File("teststats.xml"));
		printTable(doc, new File("teststats.tex"));
	}
	
	
	public static void printTable(Document doc, File file) throws IOException {
		FileWriter writer = new FileWriter(file);
		writer.write("\\begin{tabular}{|l|r|r|}\\hline\n");
		writer.write("TestCase & \\#Tests & \\#Assertions\\\\\\hline\\hline\n");
		
		List<String> types = new ArrayList<String>();
		Map<String, Integer> methods = new HashMap<String, Integer>();
		Map<String, Integer> asserts = new HashMap<String, Integer>();
		Set<String> ignored = new HashSet<String>();
		
		//ignored, testSuite, testMethods, assertions 
		ElementFilter filter = new ElementFilter("row");
		Iterator list =  doc.getDescendants(filter);
		while (list.hasNext()) {
			Element row = (Element) list.next();
			String type = ((Element)row.getContent(3)).getAttributeValue("label");
			if (((Element)row.getContent(1)).getAttributeValue("value").equals("true")) {
				ignored.add(type);
			}
			if (!types.equals("AttributesTest")) { 
				types.add(type);
				methods.put(type, Integer.parseInt(((Element)row.getContent(5)).getAttributeValue("value")));
				asserts.put(type, Integer.parseInt(((Element)row.getContent(7)).getAttributeValue("value")));
			}
		}

		Collections.sort(types);
		
		int methodsTotal = 0;
		int assertsTotal = 0;
		int ignoredMethodsTotal = 0;
		int ignoredAssertsTotal = 0;
		for (String type: types) {
			if (ignored.contains(type)) {
				ignoredMethodsTotal += methods.get(type);
				ignoredAssertsTotal += asserts.get(type);
			}
			else {
				methodsTotal += methods.get(type);
				assertsTotal += asserts.get(type);
				writer.write(type + " & " + methods.get(type) + " & " + asserts.get(type) + "\\\\\\hline\n");
			}
		}
		writer.write("\\hline\n" + "Subtotal: & " + methodsTotal + " & " + assertsTotal + "\\\\\\hline\n");
		writer.write("Ignored: & " + ignoredMethodsTotal + " & " + ignoredAssertsTotal + "\\\\\\hline\n");
		writer.write("Total: & " + (ignoredMethodsTotal + methodsTotal) + " & " + 
					(ignoredAssertsTotal + assertsTotal) + "\\\\\\hline\n");
		writer.write("\\end{tabular}\n");
		writer.flush();
		writer.close();
	}
}
