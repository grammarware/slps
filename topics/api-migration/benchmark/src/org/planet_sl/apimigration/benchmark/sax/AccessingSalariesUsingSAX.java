package org.planet_sl.apimigration.benchmark.sax;

import java.io.FileReader;
import java.io.IOException;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

public class AccessingSalariesUsingSAX {

	private String filename;
	
	public AccessingSalariesUsingSAX(String filename) {
		this.filename = filename;
	}

	private static class SumSalaries extends DefaultHandler {
		private boolean isSalary = false;
		private double sum = 0;

		public void startElement(String uri, String name, String qName, Attributes atts) {
			isSalary = name.equals("salary");
		}
		public void endElement(String uri, String name, String qName) {
			isSalary = false;
		}

		public void characters(char ch[], int start, int length) {
			if (isSalary) {
				String str = String.valueOf(ch, start, length);
				double salary = Double.parseDouble(str);
				sum += salary;
			}
		}
		
	}

	public double sum() {
		try {
			XMLReader reader = XMLReaderFactory.createXMLReader();
			SumSalaries handler = new SumSalaries();
			reader.setContentHandler(handler);
			reader.setErrorHandler(handler);
			reader.parse(new InputSource(new FileReader(filename)));
			return handler.sum;
		}
		catch (SAXException e) {
			throw new RuntimeException("sax exception");
		}
		catch (IOException e) {
			throw new RuntimeException("io exception");
		}
	}


}
