package org.planet_sl.apimigration.examples;

import java.io.IOException;

import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;

public class DetachingIssues {
	
	public static void main(String args[]) throws IOException {
		jdomCloning(); System.out.println(); System.out.println();
		jdomDetaching(); System.out.println(); System.out.println();
		xomCopying(); System.out.println(); System.out.println();
		xomDetaching(); System.out.println(); System.out.println();
		xomNoDetaching(); System.out.println(); System.out.println();
		dom4jCopying(); System.out.println(); System.out.println();
		dom4jDetaching(); System.out.println(); System.out.println();
		dom4jNoDetaching(); System.out.println(); System.out.println();
		domImporting(); System.out.println(); System.out.println();
		domDeepImporting(); System.out.println(); System.out.println();
		domNoImporting(); System.out.println(); System.out.println();
	}
	
	public static void jdomCloning() throws IOException {
		org.jdom.Element root1 = new org.jdom.Element("todo");
		org.jdom.Element kid = new org.jdom.Element("item");
		root1.addContent(kid);
		org.jdom.Document doc1 = new org.jdom.Document(root1);
		
		org.jdom.Element root2 = new org.jdom.Element("todo");
		org.jdom.Document doc2 = new org.jdom.Document(root2);
		root2.addContent((org.jdom.Element)kid.clone());
		System.out.println("JDOM Cloning 1: ");
		new org.jdom.output.XMLOutputter().output(doc1, System.out);
		System.out.println("JDOM Cloning 2: ");
		new org.jdom.output.XMLOutputter().output(doc2, System.out);
		
		System.out.println("todo/item == kid: " + (root2.getContent(0) == kid));
	}

	public static void jdomDetaching() throws IOException {
		org.jdom.Element root1 = new org.jdom.Element("todo");
		org.jdom.Element kid = new org.jdom.Element("item");
		root1.addContent(kid);
		org.jdom.Document doc1 = new org.jdom.Document(root1);
		
		org.jdom.Element root2 = new org.jdom.Element("todo");
		org.jdom.Document doc2 = new org.jdom.Document(root2);
		root2.addContent(kid.detach());
		System.out.println("JDOM Detaching 1: ");
		new org.jdom.output.XMLOutputter().output(doc1, System.out);
		System.out.println("JDOM Detaching 2: ");
		new org.jdom.output.XMLOutputter().output(doc2, System.out);
		
		System.out.println("todo/item == kid: " + (root2.getContent(0) == kid));
	}
	
	public static void xomCopying() throws IOException {
		nu.xom.Element root1 = new nu.xom.Element("todo");
		nu.xom.Element kid = new nu.xom.Element("item");
		root1.appendChild(kid);
		nu.xom.Document doc1 = new nu.xom.Document(root1);
		
		nu.xom.Element root2 = new nu.xom.Element("todo");
		nu.xom.Document doc2 = new nu.xom.Document(root2);
		root2.appendChild(kid.copy());
		System.out.println("XOM Copying 1: ");
		System.out.println(doc1.toXML());
		System.out.println("XOM Copying 2: ");
		System.out.println(doc2.toXML());
		
		System.out.println("todo/item == kid: " + (root2.getChild(0) == kid));
	}

	public static void xomDetaching() throws IOException {
		nu.xom.Element root1 = new nu.xom.Element("todo");
		nu.xom.Element kid = new nu.xom.Element("item");
		root1.appendChild(kid);
		nu.xom.Document doc1 = new nu.xom.Document(root1);
		
		nu.xom.Element root2 = new nu.xom.Element("todo");
		nu.xom.Document doc2 = new nu.xom.Document(root2);
		kid.detach();
		root2.appendChild(kid);
		System.out.println("XOM Detaching 1: ");
		System.out.println(doc1.toXML());
		System.out.println("XOM Detaching 2: ");
		System.out.println(doc2.toXML());
		
		System.out.println("todo/item == kid: " + (root2.getChild(0) == kid));
	}
	
	public static void xomNoDetaching() throws IOException {
		nu.xom.Element root1 = new nu.xom.Element("todo");
		nu.xom.Element kid = new nu.xom.Element("item");
		root1.appendChild(kid);
		@SuppressWarnings("unused")
		nu.xom.Document doc1 = new nu.xom.Document(root1);
		nu.xom.Element root2 = new nu.xom.Element("todo");
		@SuppressWarnings("unused")
		nu.xom.Document doc2 = new nu.xom.Document(root2);
		try {
			root2.appendChild(kid);
		}
		catch (nu.xom.MultipleParentException e) {
			return;
		}
		throw new AssertionError("moving childs without detaching should throw exception");
	}
	
	public static void dom4jDetaching() throws IOException {
		org.dom4j.DocumentFactory fact = new org.dom4j.DocumentFactory();
		org.dom4j.Element root1 = fact.createElement("todo");
		org.dom4j.Element kid = fact.createElement("item");
		root1.add(kid);
		org.dom4j.Document doc1 = fact.createDocument(root1);
		
		org.dom4j.Element root2 = fact.createElement("todo");
		org.dom4j.Document doc2 = fact.createDocument(root2);
		root2.add(kid.detach());
		System.out.println("DOM4J Detaching 1: ");
		System.out.println(doc1.asXML());
		System.out.println("DOM4J Detaching 2: ");
		System.out.println(doc2.asXML());
		
		System.out.println("todo/item == kid: " + (root2.node(0) == kid));
	}
	
	public static void dom4jCopying() throws IOException {
		org.dom4j.DocumentFactory fact = new org.dom4j.DocumentFactory();
		org.dom4j.Element root1 = fact.createElement("todo");
		org.dom4j.Element kid = fact.createElement("item");
		root1.add(kid);
		org.dom4j.Document doc1 = fact.createDocument(root1);
		
		org.dom4j.Element root2 = fact.createElement("todo");
		org.dom4j.Document doc2 = fact.createDocument(root2);
		root2.add(kid.createCopy());
		System.out.println("DOM4J Copying 1: ");
		System.out.println(doc1.asXML());
		System.out.println("DOM4J Copying 2: ");
		System.out.println(doc2.asXML());
		
		System.out.println("todo/item == kid: " + (root2.node(0) == kid));
	}
	
	public static void dom4jNoDetaching() throws IOException {
		org.dom4j.DocumentFactory fact = new org.dom4j.DocumentFactory();
		org.dom4j.Element root1 = fact.createElement("todo");
		org.dom4j.Element kid = fact.createElement("item");
		root1.add(kid);
		@SuppressWarnings("unused")
		org.dom4j.Document doc1 = fact.createDocument(root1);
		
		org.dom4j.Element root2 = fact.createElement("todo");
		@SuppressWarnings("unused")
		org.dom4j.Document doc2 = fact.createDocument(root2);
		try {
			root2.add(kid);
		}
		catch (org.dom4j.IllegalAddException e) {
			return;
		}
		throw new AssertionError("moving childs without detaching should throw exception");
	}

	public static void domImporting() throws IOException {
		javax.xml.parsers.DocumentBuilder b;
		try {
			javax.xml.parsers.DocumentBuilderFactory f = javax.xml.parsers.DocumentBuilderFactory.newInstance();
			f.setNamespaceAware(true);
		    b = f.newDocumentBuilder();
		} catch (javax.xml.parsers.ParserConfigurationException e) {
			e.printStackTrace();
			throw new RuntimeException("parser configuration exception");
		}
		org.w3c.dom.DOMImplementation domImpl = b.getDOMImplementation();
		
		org.w3c.dom.Document doc1 = domImpl.createDocument(null, "todo", null);
		
		org.w3c.dom.Element kid = doc1.createElement("item");
		doc1.getDocumentElement().appendChild(kid);
		
		org.w3c.dom.Document doc2 = domImpl.createDocument(null, "todo", null);
		org.w3c.dom.Node kid2 = doc2.importNode(kid, false);
		doc2.getDocumentElement().appendChild(kid2);
		
		System.out.println("DOM Importing 1: ");
		XMLSerializer serializer = new XMLSerializer(System.out, new OutputFormat(doc1));
		serializer.serialize(doc1);
		System.out.println();
		
		System.out.println("DOM Importing 2: ");
		serializer = new XMLSerializer(System.out, new OutputFormat(doc2));
		serializer.serialize(doc2);
		System.out.println();
		
		System.out.println("todo/item == kid: " + (doc2.getDocumentElement().getFirstChild() == kid));
	}
	
	public static void domDeepImporting() throws IOException {
		javax.xml.parsers.DocumentBuilder b;
		try {
			javax.xml.parsers.DocumentBuilderFactory f = javax.xml.parsers.DocumentBuilderFactory.newInstance();
			f.setNamespaceAware(true);
		    b = f.newDocumentBuilder();
		} catch (javax.xml.parsers.ParserConfigurationException e) {
			e.printStackTrace();
			throw new RuntimeException("parser configuration exception");
		}
		org.w3c.dom.DOMImplementation domImpl = b.getDOMImplementation();
		
		org.w3c.dom.Document doc1 = domImpl.createDocument(null, "todo", null);
		
		org.w3c.dom.Element kid = doc1.createElement("item");
		doc1.getDocumentElement().appendChild(kid);
		
		org.w3c.dom.Document doc2 = domImpl.createDocument(null, "todo", null);
		org.w3c.dom.Node kid2 = doc2.importNode(kid, true);
		doc2.getDocumentElement().appendChild(kid2);
		
		System.out.println("DOM Importing 1: ");
		XMLSerializer serializer = new XMLSerializer(System.out, new OutputFormat(doc1));
		serializer.serialize(doc1);
		System.out.println();
		
		System.out.println("DOM Importing 2: ");
		serializer = new XMLSerializer(System.out, new OutputFormat(doc2));
		serializer.serialize(doc2);
		System.out.println();
		
		System.out.println("todo/item == kid: " + (doc2.getDocumentElement().getFirstChild() == kid));
	}
	
	public static void domNoImporting() throws IOException {
		javax.xml.parsers.DocumentBuilder b;
		try {
			javax.xml.parsers.DocumentBuilderFactory f = javax.xml.parsers.DocumentBuilderFactory.newInstance();
			f.setNamespaceAware(true);
		    b = f.newDocumentBuilder();
		} catch (javax.xml.parsers.ParserConfigurationException e) {
			e.printStackTrace();
			throw new RuntimeException("parser configuration exception");
		}
		org.w3c.dom.DOMImplementation domImpl = b.getDOMImplementation();
		
		org.w3c.dom.Document doc1 = domImpl.createDocument(null, "todo", null);
		
		org.w3c.dom.Element kid = doc1.createElement("item");
		doc1.getDocumentElement().appendChild(kid);
		
		org.w3c.dom.Document doc2 = domImpl.createDocument(null, "todo", null);
		try {
			doc2.getDocumentElement().appendChild(kid);
		}
		catch (org.w3c.dom.DOMException e) {
			return;
		}
		throw new AssertionError("no exception thrown when appending non-imported node");
	}
}

