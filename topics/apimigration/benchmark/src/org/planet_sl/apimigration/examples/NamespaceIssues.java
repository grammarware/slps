package org.planet_sl.apimigration.examples;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;


public class NamespaceIssues {
	public static void main(String args[]) throws UnsupportedEncodingException, IOException {
		dom4j();
		xom();
		dom();
		
		System.out.println("**********************");
		
		jdomWithURI();
		dom4jWithURI();
		xomWithURI();
		domWithURI();
	}

	
	
	public static void xom() {
		// XOM dynamically enforces presence, non-nullness & non-emptyness of URI if prefix is given
		nu.xom.Element root = new nu.xom.Element("todo");
		try {
			root.setNamespacePrefix("todo");
		}
		catch (nu.xom.NamespaceConflictException e) {
			System.out.println("XOM:");
			System.out.println(e.getMessage());
		}		
	}
	
	public static void dom4j() throws UnsupportedEncodingException, IOException {
		// DOM4J allows construction of "invalid" (?) XML document
		org.dom4j.DocumentFactory fact = new org.dom4j.DocumentFactory();
		org.dom4j.Element root = fact.createElement("todo:todo");
		org.dom4j.Document doc = fact.createDocument(root);
		System.out.println("DOM4J: ");
		System.out.println(doc.asXML());
	}
	
	public static void dom() throws IOException {
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
		
		// DOM works similar to DOM4J
		org.w3c.dom.Document doc = domImpl.createDocument("", "todo:todo", null);
		System.out.println("DOM: ");
		XMLSerializer serializer = new XMLSerializer(System.out, new OutputFormat(doc));
		serializer.serialize(doc);
		System.out.println();
	}
	
	///////////////////////////////
	
	public static void jdomWithURI() {
		// JDOM statically requires URI if prefix is given 
		// and dynamically ensures it's non-null & non-empty
		org.jdom.Element root = new org.jdom.Element("todo", "todo", "http://www.example.com");
		org.jdom.Document doc = new org.jdom.Document(root);
		System.out.println("JDOM: ");
		System.out.println(new org.jdom.output.XMLOutputter().outputString(doc));
	}
	
	public static void xomWithURI() {
		nu.xom.Element root = new nu.xom.Element("todo");
		
		// Setting URI and prefix has the consequence that the namespace declaration is added
		root.setNamespaceURI("http://www.example.com"); 
		root.setNamespacePrefix("todo");
		nu.xom.Document doc = new nu.xom.Document(root);
		System.out.println("XOM:");
		System.out.println(doc.toXML());
	}
	
	public static void dom4jWithURI() throws UnsupportedEncodingException, IOException {
		org.dom4j.DocumentFactory fact = new org.dom4j.DocumentFactory();
		org.dom4j.Element root = fact.createElement("todo:todo"/*, "http://www.example.com" */);
		
		// Namespace decl should be declared explicitly
		root.addNamespace("todo", "http://www.example.com");
		org.dom4j.Document doc = fact.createDocument(root);
		System.out.println("DOM4J: ");
		System.out.println(doc.asXML());
	}
	
	public static void domWithURI() throws IOException {
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
		
		// DOM works similar to DOM4J; however you must provide an URI if a name has a prefix
		// passing in the the URI with createDocument makes no difference.
		org.w3c.dom.Document doc = domImpl.createDocument("", "todo:todo", null);
		
		// DOM does not add the declaration by default.
		doc.getDocumentElement().setAttribute("xmlns:todo", "http://www.example.com");
		System.out.println("DOM: ");
		XMLSerializer serializer = new XMLSerializer(System.out, new OutputFormat(doc));
		serializer.serialize(doc);
		System.out.println();
	}
}
