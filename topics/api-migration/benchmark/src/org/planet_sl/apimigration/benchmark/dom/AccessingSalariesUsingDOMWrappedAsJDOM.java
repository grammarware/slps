package org.planet_sl.apimigration.benchmark.dom;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.planet_sl.apimigration.benchmark.dom.wrapped_as_jdom.Document;
import org.planet_sl.apimigration.benchmark.dom.wrapped_as_jdom.Element;
import org.planet_sl.apimigration.benchmark.dom.wrapped_as_jdom.ElementFilter;
import org.xml.sax.SAXException;


public class AccessingSalariesUsingDOMWrappedAsJDOM {

	private Document document;

	public AccessingSalariesUsingDOMWrappedAsJDOM(String filename) throws SAXException, IOException, ParserConfigurationException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setValidating(false);
		//document = new Document(factory.newDocumentBuilder().parse(new File(filename)));
	}	
	
	@SuppressWarnings("unchecked")
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

	@SuppressWarnings("unchecked")
	public void raise(double amount) {
		ElementFilter filter = new ElementFilter("salary");
		Iterator iter = document.getDescendants(filter);
//		List<Element> list = new LinkedList<Element>();
		while (iter.hasNext()) {
			Element elt = (Element)iter.next();
			Double newSalary = Double.parseDouble(elt.getText()) + amount;
			elt.setText(newSalary.toString());
			// Updating here throws concurrent modification exception
			//list.add(elt);
		}
//		for (Element elt: list) {
//			Double newSalary = Double.parseDouble(elt.getText()) + amount;
//			elt.setText(newSalary.toString());
//		}
	}
}
