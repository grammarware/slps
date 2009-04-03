package org.planet_sl.apimigration.benchmark.jdom;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.filter.ElementFilter;
import org.jdom.input.SAXBuilder;

@SuppressWarnings("unchecked")
public class AccessingSalariesUsingJDOM {

	private Document document;
	
	public AccessingSalariesUsingJDOM(String filename) throws JDOMException, IOException {
		SAXBuilder builder = new SAXBuilder(false); 
		document = builder.build(new File(filename));
	}
		
	public double sum() {
		double total = 0;
		ElementFilter filter = new ElementFilter("salary");
		Iterator iter = document.getDescendants(filter);
		while (iter.hasNext()) {
			Element elt = (Element)iter.next();
			total += Double.parseDouble(elt.getText());
		}
		return total;
	}

	public void raise(double amount) {
		ElementFilter filter = new ElementFilter("salary");
		List list = document.getContent(filter);
		System.out.println(list);
		for (Object o: list) {
			Element elt = (Element)o;
			Double newSalary = Double.parseDouble(elt.getText()) + amount;
			elt.setText(newSalary.toString());
		}
	}

	
}
