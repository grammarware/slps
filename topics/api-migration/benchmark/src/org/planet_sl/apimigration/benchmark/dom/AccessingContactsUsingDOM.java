package org.planet_sl.apimigration.benchmark.dom;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.apache.xpath.XPathAPI;
import org.jdom.JDOMException;
import org.planet_sl.apimigration.benchmark.scenarios.accessing.Resources;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;


public class AccessingContactsUsingDOM {

	private Document doc;

	public AccessingContactsUsingDOM(String filename) throws SAXException, IOException, ParserConfigurationException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setValidating(false);
		doc = factory.newDocumentBuilder().parse(new File(filename));
	}
	
	public static void main(String arg[]) throws JDOMException, IOException, SAXException, ParserConfigurationException {
		new AccessingContactsUsingDOM(Resources.CONTACTS_XML).august29();
	}
	
	public float average() {
		int total = 0;
		int count = 0;
		NodeList nodelist = doc.getElementsByTagName("age");
		for (int i = 0; i < nodelist.getLength(); i++) {
			Element elem = (Element) nodelist.item(i);
			total += Integer.parseInt(elem.getTextContent());
			count++;
		}
		return (float) total / (float) count;
	}
	
	public float averageXPath() {
		int total = 0;
		int count = 0;
		try {
			NodeList nodelist = XPathAPI.selectNodeList(doc, "//age");
			for (int i = 0; i < nodelist.getLength(); i++) {
				Element elem = (Element) nodelist.item(i);
				total += Integer.parseInt(elem.getTextContent());
				count++;
			}
		} catch (TransformerException e) {
			e.printStackTrace();
			throw new RuntimeException("transformer exception");
		}
		return (float) total / (float) count;
	}	
	
	public void august29() {
		try {
			NodeList nodelist = XPathAPI.selectNodeList(doc, "/*/person");
			for (int i = 0; i < nodelist.getLength(); i++) {
				Element px = (Element)nodelist.item(i);
				Element namex = (Element)XPathAPI.selectNodeList(px, "name").item(0);
				Element agex = (Element)XPathAPI.selectNodeList(px, "age").item(0);
				if (namex.getTextContent().equals("John McCain")) {
					int age = Integer.parseInt(agex.getTextContent());
					age++;
					agex.setTextContent(Integer.toString(age));
					break;					
				}				
			}
		} catch (TransformerException e) {
			e.printStackTrace();
			throw new RuntimeException("transformer exception");
		}
	}
	
	
	
}
