package org.planet_sl.apimigration.benchmark.analysis;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.jdom.*;
import org.jdom.filter.ElementFilter;
import org.jdom.input.SAXBuilder;

public class XML2LaTeXTable {

	public static void main(String args[]) throws JDOMException, IOException {
		SAXBuilder builder = new SAXBuilder();
		Document doc = builder.build(new File("apistats.xml"));
		printTableForAPI("org.jdom", doc, new File("jdomapi.tex"));
		printTableForAPI("nu.xom", doc, new File("xomapi.tex"));
	}
	
	public static void printTableForAPI(String api, Document doc, File file) throws IOException {
		FileWriter writer = new FileWriter(file);
		writer.write("\\begin{tabular}{|l|r|r|}\\hline\n");
		writer.write(api + " & NCLOC & \\#Features\\\\\\hline\\hline\n");
		
		List<String> types = new ArrayList<String>();
		Map<String, Integer> ncloc = new HashMap<String, Integer>();
		Map<String, Integer> features = new HashMap<String, Integer>();
		
		ElementFilter filter = new ElementFilter("row");
		Iterator list =  doc.getDescendants(filter);
		while (list.hasNext()) {
			Element row = (Element) list.next();
			if (((Element)row.getContent(1)).getAttributeValue("value").equals(api)) {
				String type = ((Element)row.getContent(3)).getAttributeValue("label");
				types.add(type);
				ncloc.put(type, Integer.parseInt(((Element)row.getContent(5)).getAttributeValue("value")));
				features.put(type, Integer.parseInt(((Element)row.getContent(7)).getAttributeValue("value")));
			}
			
		}

		Collections.sort(types);
		
		int locTotal = 0;
		int featureTotal = 0;
		for (String type: types) {
			locTotal += ncloc.get(type);
			featureTotal += features.get(type);
			writer.write(type + " & " + ncloc.get(type) + " & " + features.get(type) + "\\\\\\hline\n");
		}
		writer.write("\\hline\n" + " & " + locTotal + " & " + featureTotal + "\\\\\\hline\n");
		writer.write("\\end{tabular}\n");
		writer.flush();
		writer.close();
	}
	
	
}
