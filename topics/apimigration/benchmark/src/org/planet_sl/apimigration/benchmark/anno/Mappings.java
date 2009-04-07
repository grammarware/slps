package org.planet_sl.apimigration.benchmark.anno;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;



public class Mappings {

	private static final String PKG = "org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom";
	
	private static final String CLASSES[] = {"Attribute",
			"Builder",
			"XPathContext",
			"Comment",
			"DocType",
			"Document",
			"Element",
			"Elements",
			"IllegalAddException",
			"IllegalCharacterDataException",
			"IllegalDataException",
			"IllegalNameException",
			"IllegalTargetException",
			"LocatorFilter",
			"MalformedURIException",
			"MultipleParentException",
			"ValidityException",
			"WellformednessException",
			"XMLException",
			"XPathException",
			"Namespace",
			"NamespaceConflictException",
			"Node",
			"NodeFactory",
			"Nodes",
			"NoSuchAttributeException",
			"NoSuchChildException",
			"ParentNode",
			"ParentNodeTest",
			"ParsingException",
			"ProcessingInstruction",
			"Serializer",
			"Text",
			"Utils"};
	
	public static void main(String arg[]) throws ClassNotFoundException {
		classMappings();
		methodMappings();
	}
	
	public static void classMappings() throws ClassNotFoundException {
		for (String s: CLASSES) {
			Class<?> klass = Class.forName(PKG + "." + s);
			Annotation[] annos = klass.getAnnotations();
			for (Annotation anno: annos) {
				if (anno.annotationType().equals(MapsTo.class)) {
					System.out.println("\"nu.xom." + s + "\", \"" + ((MapsTo)anno).value() + "\"");
				}
			}
		}
	}
	
	public static void methodMappings() throws ClassNotFoundException {
		for (String s: CLASSES) {
			Class<?> klass = Class.forName(PKG + "." + s);
			for (Method method: klass.getDeclaredMethods()) {
				for (Annotation anno: method.getAnnotations()) {
					if (anno.annotationType().equals(MapsTo.class)) {
						System.out.println("\"nu.xom." + s + "#" + method.getName() + "(" + 
								paramTypesToString(method.getParameterTypes()) + ")" + "\", \"" + ((MapsTo)anno).value() + "\"");
					}
				}
			}
			for (Constructor<?> cons: klass.getDeclaredConstructors()) {
				for (Annotation anno: cons.getAnnotations()) {
					if (anno.annotationType().equals(MapsTo.class)) {
						System.out.println("\"nu.xom." + s + "(" + 
								paramTypesToString(cons.getParameterTypes()) + ")" + "\", \"" + ((MapsTo)anno).value() + "\"");
					}
				}
			}
		}
	}

	public static String paramTypesToString(Class<?> classes[]) {
		StringBuilder s = new StringBuilder();
		for (int i = 0; i < classes.length; i++) {
			s.append(classes[i].getSimpleName());
			if (i < classes.length - 1) {
				s.append(", ");
			}
		}
		return s.toString();
	}
}
