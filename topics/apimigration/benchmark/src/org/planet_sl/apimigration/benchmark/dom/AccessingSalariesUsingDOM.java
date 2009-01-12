package org.planet_sl.apimigration.benchmark.dom;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.apache.xpath.XPathAPI;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class AccessingSalariesUsingDOM {

	private Document document;

	public AccessingSalariesUsingDOM(String filename) throws SAXException, IOException, ParserConfigurationException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setValidating(false);
		document = factory.newDocumentBuilder().parse(new File(filename));
	}
	
	public double sum() {
		String xpath = "//salary";
		double total = 0;
		NodeList nodelist;
		try {
			nodelist = XPathAPI.selectNodeList(document, xpath);
			for (int i = 0; i < nodelist.getLength(); i++) {
				Element elem = (Element) nodelist.item(i);
				total += Double.parseDouble(elem.getTextContent());
			}
		} catch (TransformerException e) {
			e.printStackTrace();
			throw new RuntimeException("transformer exception");
		}
		return total;
	}
	
	public void raise(double amount) {
		try {
			String query = "//salary";
			NodeList nodelist = XPathAPI.selectNodeList(document, query);
			for (int i = 0; i < nodelist.getLength(); i++) {
				Element elem = (Element) nodelist.item(i);
				Double salary = Double.parseDouble(elem.getTextContent()) + amount;
				elem.setTextContent(salary.toString());
			}
		} catch (TransformerException e) {
			e.printStackTrace();
			throw new RuntimeException("transformer exception");
		}
	}
	
	
	
}
