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
public class AccessingContactsUsingJDOM {

	private Document doc;
	
	public AccessingContactsUsingJDOM(String filename) throws JDOMException, IOException {
		SAXBuilder builder = new SAXBuilder(false); 
		doc = builder.build(new File(filename));
	}
	
	
	public float average() {
		int total = 0;
		int count = 0;
		ElementFilter filter = new ElementFilter("age");
		Iterator i = doc.getDescendants(filter);
		while (i.hasNext()) {
			Element e = (Element)i.next();
			total += Integer.parseInt(e.getText());
			count++;
		}
		return (float) total / (float) count;
	}

	public void august29() {
		ElementFilter filter = new ElementFilter("person");
		List l = doc.getRootElement().getContent(filter); 
		for (Object o : l) {
			Element px = (Element)o;
			Element namex = px.getChild("name");
			Element agex = px.getChild("age");
			if (namex.getText().equals("John McCain")) {
				int age = Integer.parseInt(agex.getText());
				age++;
				agex.setText(Integer.toString(age));
				break;
			}
		}
	}

	public void august29b() {
		ElementFilter filter = new ElementFilter("person");
		Iterator i = doc.getDescendants(filter); 
		while (i.hasNext()) {
			Element px = (Element)i.next();
			Element namex = px.getChild("name");
			Element agex = px.getChild("age");
			if (namex.getText().equals("John McCain")) {
				int age = Integer.parseInt(agex.getText());
				age++;
				agex.setText(Integer.toString(age));
				break;
			}
		}
	}
	
	
	
}
