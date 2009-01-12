package org.planet_sl.apimigration.benchmark.dom;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.ContactsToDocument;
import org.planet_sl.apimigration.benchmark.scenarios.xo_mapping.Person;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;


public class ContactsToDocumentUsingDOM extends ContactsToDocument {
	
	public static void main(String args[]) throws IOException {
//		DocumentBuilder b;
//		try {
//			DocumentBuilderFactory f = DocumentBuilderFactory.newInstance();
//		    b = f.newDocumentBuilder();
//		} catch (ParserConfigurationException e) {
//			e.printStackTrace();
//			throw new RuntimeException("parser configuration exception");
//		}
//		Document doc = b.getDOMImplementation().createDocument(null, "contacts", null);
//		Element root = doc.getDocumentElement();
//		Element elt = doc.createElement("bla");
//		root.appendChild(elt);
//		elt.setTextContent("dit is content");
//		System.out.println(elt.getTextContent());
		
		new ContactsToDocumentUsingDOM(Person.PERSONS).save(System.out);
	}
	
	public ContactsToDocumentUsingDOM(List<Person> contacts) {
		this.contacts = contacts;
	}
	
	public void save(OutputStream output) throws IOException {
		Document document = makeDocument();
		OutputFormat format = new OutputFormat(document);
		XMLSerializer serializer = new XMLSerializer(output, format);
		serializer.serialize(document);
	}
	
	/*
	 * document(?doc) :- try ...catch ....
	 * root(?root) :- document(?doc), doc?.getDocumentElement()
	 * level1(?elt) :- root(?root), document(?doc), ?elt = ?doc.createElement(), ?root.appendChild(?elt)
	 */

	public Document makeDocument() {
		DocumentBuilder b;
		try {
			DocumentBuilderFactory f = DocumentBuilderFactory.newInstance();
		    b = f.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
			throw new RuntimeException("parser configuration exception");
		}
		Document doc = b.getDOMImplementation().createDocument(null, "contacts", null);
		Element root = doc.getDocumentElement();
		for (Person p: contacts) {
			// XML element for person
			Element px = doc.createElement("person");
			
			// Name of person
			Element namex = doc.createElement("name");
			namex.setTextContent(p.getName());			

			// Age of person
			Element agex = doc.createElement("age");
			agex.setTextContent(Integer.toString(p.getAge()));

			px.appendChild(namex);
			px.appendChild(agex);

			root.appendChild(px);
		}
		return doc;
	}

	public Document makeDocument_AppendingLater() {
		Document document;
		try {
			DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
			document = documentBuilder.getDOMImplementation().createDocument(null, "contacts", null);
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
			throw new RuntimeException("parser configuration exception");
		}
		Element root = document.getDocumentElement();
		for (Person p: contacts) {
			Element elt = document.createElement("person");
			Element nameElt = document.createElement("name");
			Node name = document.createTextNode(p.getName());
			Element ageElt = document.createElement("age");
			Node age = document.createTextNode(new Integer(p.getAge()).toString());
			
			root.appendChild(elt);
			elt.appendChild(nameElt);
			elt.appendChild(ageElt);
			nameElt.appendChild(name);
			ageElt.appendChild(age);
			
		}
		return document;
	}

}
