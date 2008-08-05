package slps.java2bgf;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;
import javax.xml.parsers.*;
import javax.xml.transform.*;
import javax.xml.transform.dom.*;
import javax.xml.transform.stream.*;
import org.w3c.dom.*;

public class Tool {

	private static Document doc;
	private static Element root;

	public static void main(String[] args) throws Exception {

		String dirname = args[0]; // the basedir for the classes to be scanned
		String pkgname = args[1]; // the package holding the relevant classes
		String bgf = args[2]; // the output file for the extracted grammar

		// Create the DOM container to hold the result of grammar extraction
		DocumentBuilderFactory dbfac = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = dbfac.newDocumentBuilder();
		doc = docBuilder.newDocument();
		root = doc.createElement("bgf:grammar");
		root.setAttribute("xmlns:bgf", "http://planet-sl.org/bgf");
		doc.appendChild(root);

		// Loading all relevant classes
		File dir = new File(dirname);
		if (!dir.exists())
			throw new RuntimeException("Can't find directory " + dirname + "!");
		String pkgdirname = dirname + File.separatorChar + pkgname;
		File pkgdir = new File(pkgdirname);
		if (!pkgdir.exists())
			throw new RuntimeException("Can't find package directory "
					+ pkgdirname + "!");
		Collection<Class<?>> classes = new LinkedList<Class<?>>();
		for (String f : pkgdir.list()) {
			int len = f.length();
			if (len > 5 && f.substring(len - 6).equals(".class")) {
				String classname = pkgname + "." + f.substring(0, len - 6);
				try {
					Class<?> clss = Class.forName(classname);
					System.out.println("Loaded " + clss.getName() + ".");
					classes.add(clss);
				} catch (ClassNotFoundException e) {
					throw new RuntimeException("Can't load class " + classname
							+ "!");
				}
			}
		}
		
		if (classes.isEmpty())
			throw new RuntimeException("No classes found!");

		// Generate one nonterminal per class
		for (Class<?> clss : classes) {
			Element rule = doc.createElement("bgf:production");
			Element nonterminal = doc.createElement("nonterminal");
			Element rhs = doc.createElement("bgf:expression");
			nonterminal.appendChild(doc.createTextNode(clss.getSimpleName()));
			root.appendChild(rule);
			rule.appendChild(nonterminal);
			rule.appendChild(rhs);
			Collection<Element> tmp = new LinkedList<Element>();

			// Handle enums vs. abstract vs. concrete classes differently
			String compositor;
			String unit;

			if (clss.isEnum()) {
				compositor = "choice";
				unit = "empty";
				for (Object c : clss.getEnumConstants()) {
					Element selectable = doc.createElement("selectable");
					tmp.add(selectable);
					Element selector = doc.createElement("selector");
					selectable.appendChild(selector);
					selector.appendChild(doc.createTextNode(c.toString()));
					Element expr = doc.createElement("bgf:expression");
					selectable.appendChild(expr);
					Element empty = doc.createElement("epsilon");
					expr.appendChild(empty);
				}
			} else if (Modifier.isAbstract(clss.getModifiers())) {
				compositor = "choice";
				unit = "empty";
				if (clss.getFields().length != 0)
					throw new RuntimeException(
							"Can't handle fields of abstract class "
									+ clss.getSimpleName() + "!");
				
				// Find subclasses
				for (Class<?> clss2 : classes)
					if (clss2.getSuperclass() == clss) {
						Element nt = doc.createElement("nonterminal");
						tmp.add(nt);
						nt.appendChild(doc
								.createTextNode(clss2.getSimpleName()));
					}
				
			} else {
				compositor = "sequence";
				unit = "epsilon";

				// Iterate over the features of a class
				Collection<Feature> fs = new LinkedList<Feature>();
				for (Field f : clss.getFields())
					fs.add(new Feature(f.getName(),f.getType()));
				for (Method m : clss.getMethods())
					if (m.getDeclaringClass() == clss
					&&	m.getParameterTypes().length == 0
					&&  m.getName().length() > 3
					&&  m.getName().startsWith("get"))
						fs.add(new Feature(m.getName().substring(3),m.getReturnType()));				
				
				for (Feature f : fs) {
					Element selectable = doc.createElement("selectable");
					tmp.add(selectable);
					Element selector = doc.createElement("selector");
					selectable.appendChild(selector);
					selector.appendChild(doc.createTextNode(f.getName()));
					Element expr = doc.createElement("bgf:expression");
					selectable.appendChild(expr);
					if (f.getType() == LinkedList.class 
					|| f.getType() == List.class) {
						Element coll = doc.createElement("star");
						expr.appendChild(coll);
						Element inner = doc.createElement("bgf:expression");
						coll.appendChild(inner);
						inner.appendChild(doc.createElement("any"));						
					}
					else
					{
						Element nt = doc.createElement("nonterminal");
						expr.appendChild(nt);
						nt.appendChild(doc.createTextNode(f.getType().getSimpleName()));
						expr.appendChild(nt);
					}
				}
			}
			if (tmp.size() == 0)
				rhs.appendChild(doc.createElement(unit));
			else if (tmp.size() == 1)
				rhs.appendChild(tmp.iterator().next());
			else {
				Element sequence = doc.createElement(compositor);
				rhs.appendChild(sequence);
				for (Element e : tmp) {
					Element wrapper = doc.createElement("bgf:expression");
					sequence.appendChild(wrapper);
					wrapper.appendChild(e);
				}
			}
		}

		// Write the DOM container to the output file
		Transformer trans = TransformerFactory.newInstance().newTransformer();
		trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
		trans.setOutputProperty(OutputKeys.INDENT, "yes");
		trans.transform(new DOMSource(doc), new StreamResult(
				new FileOutputStream(bgf)));

	}
	
	static class Feature {
		private String name;
		private Class<?> type;
		public Feature(String name, Class<?> type) {
			this.name = name;
			this.type = type;
		}
		public String getName() { return name; }
		public Class<?> getType() { return type; }
	}
	
}
