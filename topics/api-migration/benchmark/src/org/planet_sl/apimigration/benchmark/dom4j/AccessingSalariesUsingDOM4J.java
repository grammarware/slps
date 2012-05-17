package org.planet_sl.apimigration.benchmark.dom4j;

import java.io.File;
import java.util.List;

import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.Node;
import org.dom4j.XPath;
import org.dom4j.io.SAXReader;

public class AccessingSalariesUsingDOM4J {

	public AccessingSalariesUsingDOM4J(String filename) throws DocumentException {
		SAXReader reader = new SAXReader();
		document = reader.read(new File(filename));
	}
	
	private Document document;
	
	@SuppressWarnings("unchecked")
	public double sum_() {
		// This does not seem to work.
		XPath query = DocumentHelper.createXPath("//salary");
		List<Element> results = query.selectNodes(document);
		double total = 0;
		for (Element elt: results) {
			System.out.println(elt);
			total += Double.parseDouble(elt.getText());
		}
		return total;
	}
	
	public double sum() {
		return sum(document.getRootElement());
	}
	
	public void raise(double amount) {
		raise(document.getRootElement(), amount);
	}
	
	private double sum(Element element) {
		double total = 0;
		for (int i = 0, size = element.nodeCount(); i < size; i++ ) {
			Node node = element.node(i);
			if (node instanceof Element ) {
				Element elt = (Element)node;
				if (elt.getName().equals("salary")) {
					total += Double.parseDouble(elt.getText());
				}
				else {
					total += sum(elt);
				}
			}
		}
		return total;
	}
	
		
	private void raise(Element element, double amount) {
		for (int i = 0, size = element.nodeCount(); i < size; i++ ) {
			Node node = element.node(i);
			if (node instanceof Element ) {
				Element elt = (Element)node;
				if (elt.getName().equals("salary")) {
					Double salary = Double.parseDouble(elt.getText()) + amount;
					elt.setText(salary.toString());
				}
				else {
					raise(elt, amount);
				}
			}
		}
	}

}
