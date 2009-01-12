package org.planet_sl.apimigration.benchmark.dom.wrapped_as_jdom;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.NodeList;

public class Document {

	private org.w3c.dom.Document document;
	private Element root; 

	/*
	 * JDOM API (excerpt)
	 */
	
	public Document addContent(Element root) {
		this.root = root;
		String name = root.name;
		try {
			DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
			documentBuilderFactory.setNamespaceAware(true);
			DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
			document = documentBuilder.getDOMImplementation().createDocument(null, name, null);
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
			throw new RuntimeException("parser configuration exception");
		}
		root.build(document);
		return this;
	}
	
	@SuppressWarnings("unchecked")
	public Iterator getDescendants() {
		NodeList nodeList = document.getElementsByTagName("*");
		List<Object> list = new LinkedList<Object>();
		for (int i = 0; i < nodeList.getLength(); i++) {
			list.add(new Element((org.w3c.dom.Node)nodeList.item(i)));
		}
		return list.iterator();
	}

	
	@SuppressWarnings("unchecked")
	public Iterator getDescendants(ElementFilter filter) {
		NodeList nodeList = document.getElementsByTagName(filter.getName());
		List<Object> list = new LinkedList<Object>();
		for (int i = 0; i < nodeList.getLength(); i++) {
			list.add(new Element((org.w3c.dom.Node)nodeList.item(i)));
		}
		return list.iterator();
	}

	/*
	 * Utils
	 */
	
	public String toString() {
		return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + root.toString();
	}	
	
	
}
