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

public class PKGStats2LaTeX {

	public static void main(String args[]) throws JDOMException, IOException {
		SAXBuilder builder = new SAXBuilder();
		Document doc = builder.build(new File("pkgstats.xml"));
		printTableForAPIs("nu.xom", doc, new File("xompkgstats.tex"));
		printTableForAPIs("org.jdom", doc, new File("jdompkgstats.tex"));
	}

	public static void printTableForAPIs(String api, Document doc, File file)
			throws IOException {
		FileWriter writer = new FileWriter(file);
		writer.write("\\begin{tabular}{|l|r|}\\hline\n");
		writer.write("\\apiPackageHeading" + " & \\numOfTypesHeading \\\\\\hline\\hline\n");

		List<String> packages = new ArrayList<String>();
		Map<String, Integer> types = new HashMap<String, Integer>();

		ElementFilter filter = new ElementFilter("row");
		Iterator list = doc.getDescendants(filter);
		while (list.hasNext()) {
			Element row = (Element) list.next();
			String pkg = ((Element) row.getContent(1)).getAttributeValue("label");
			packages.add(pkg);
			types.put(pkg, Integer.parseInt(((Element) row.getContent(3))
					.getAttributeValue("value")));
		}

		Collections.sort(packages);

		int total = 0;
		for (String pkg : packages) {
			if (pkg.startsWith(api)) {
				total += types.get(pkg);
				writer.write(pkg + " & " + types.get(pkg) + "\\\\\\hline\n");
			}
		}
		writer.write("\\hline\n" + " & " + total + "\\\\\\hline\n");
		writer.write("\\end{tabular}\n");
		writer.flush();
		writer.close();
	}

}
