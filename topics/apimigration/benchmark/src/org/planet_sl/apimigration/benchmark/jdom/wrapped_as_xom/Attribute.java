package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.util.List;

import org.planet_sl.apimigration.benchmark.anno.API;
import org.planet_sl.apimigration.benchmark.anno.MapsTo;
import org.planet_sl.apimigration.benchmark.anno.API.Kind;

@SuppressWarnings("unchecked")
@MapsTo("org.jdom.Attribute")
public class Attribute extends Node {

	@MapsTo("int")
	public static class Type  {
		private int type;

		Type(int type) {
			this.type = type;
		}
		
		public static final Attribute.Type CDATA = new Type(org.jdom.Attribute.CDATA_TYPE);
		public static final Attribute.Type UNDECLARED = new Type(org.jdom.Attribute.UNDECLARED_TYPE);
		public static final Attribute.Type ID = new Type(org.jdom.Attribute.ID_TYPE);
		public static final Attribute.Type IDREF = new Type(org.jdom.Attribute.IDREF_TYPE);
		public static final Attribute.Type IDREFS = new Type(org.jdom.Attribute.IDREFS_TYPE);
		public static final Attribute.Type NMTOKEN = new Type(org.jdom.Attribute.NMTOKEN_TYPE);
		public static final Attribute.Type NMTOKENS = new Type(org.jdom.Attribute.NMTOKENS_TYPE);
		public static final Attribute.Type NOTATION = new Type(org.jdom.Attribute.NOTATION_TYPE);
		public static final Attribute.Type ENTITY = new Type(org.jdom.Attribute.ENTITY_TYPE);
		public static final Attribute.Type ENTITIES = new Type(org.jdom.Attribute.ENTITIES_TYPE);
		public static final Attribute.Type ENUMERATION = new Type(org.jdom.Attribute.ENUMERATED_TYPE);
		
		@API(value = Kind.MACRO, doc = "JDOM attribute types are just ints")
		public String getName() {
			switch (type) {
			case org.jdom.Attribute.UNDECLARED_TYPE:
				return "UNDECLARED";   
			case org.jdom.Attribute.CDATA_TYPE:
				return "CDATA";  
			case org.jdom.Attribute.ID_TYPE:
				return "ID";  
			case org.jdom.Attribute.IDREF_TYPE:
				return "IDREF";   
			case org.jdom.Attribute.IDREFS_TYPE:
				return "IDREFS";   
			case org.jdom.Attribute.NMTOKEN_TYPE:
				return "NMTOKEN";  
			case org.jdom.Attribute.NMTOKENS_TYPE:
				return "NMTOKENS";   
			case org.jdom.Attribute.NOTATION_TYPE:
				return "NOTATION";   
			case org.jdom.Attribute.ENTITY_TYPE:
				return "ENTITY";   
			case org.jdom.Attribute.ENTITIES_TYPE:
				return "ENTITIES";  
			case org.jdom.Attribute.ENUMERATED_TYPE:
				return "ENUMERATION"; 
			default: 
				throw new AssertionError(
						"bug in wrapping JDOM as XOM: unexpected attribute type: " + type); 
			}
		}
		
		public int hashCode() {
			return type;
		}
		
		public boolean equals(Object o) {
			if (!(o instanceof Type)) {
				return false;
			}
			return type == ((Type)o).type;
		}
	}
	
	
	org.jdom.Attribute attribute;
	
	Attribute(org.jdom.Attribute attribute) {
		this.attribute = attribute;
	}

	// XOM api starts here.
	
	@API(value = Kind.IDIOM, doc = "Cloning constructor vs clone() in JDOM")
	@MapsTo("org.jdom.Attribute#clone()")
	public Attribute(Attribute attribute) {
		this((org.jdom.Attribute)attribute.attribute.clone());
	}

	@API(Kind.EQUAL)
	@MapsTo("org.jdom.Attribute(String,String)")
	public Attribute(String name, String value) {
		int index = name.indexOf(":");
		String prefix = "";
		String localName = name;
		if (index != -1) {
			prefix = name.substring(0, index);
			localName = name.substring(index + 1, name.length());
		}
		if (prefix.equals("xml")) {
			throw new NamespaceConflictException("creating xml: prefix attribute without proper namespace");
		}
		if (prefix.equals("xmlns")) {
			throw new IllegalNameException("creating xmlns attribute", name);
		}
		try {
			attribute = new org.jdom.Attribute(localName, value);
		}
		catch (org.jdom.IllegalNameException e) {
			throw new IllegalNameException(e, localName);
		}
		catch (org.jdom.IllegalDataException e) {
			throw new IllegalDataException(e, value);
		}
	}

	@API(value = Kind.TYPE, doc = "XOM::Attribute.Type vs ints")
	@MapsTo("org.jdom.Attribute(String,String,int)")
	public Attribute(String localName, String value, Attribute.Type type) {
		this(new org.jdom.Attribute(localName, value, ((Type)type).type));
	}

//	@API(value = Kind.DATA, doc = "XOM allows colons in names")
//	@API(value = Kind.TYPE, doc = "JDOM::Namespace")
//	@API(value = Kind.THROWS)
	@MapsTo("org.jdom.Attribute(String,String,org.jdom.Namespace)")
	public Attribute(String name, String URI, String value) {
		int index = name.indexOf(":");
		String prefix = "";
		String localName = name;
		if (index != -1) {
			prefix = name.substring(0, index);
			localName = name.substring(index + 1, name.length());
		}
		if (prefix.equals("xml") && !URI.equals(org.jdom.Namespace.XML_NAMESPACE.getURI())) {
			throw new NamespaceConflictException("invalid namespace for xml: attribute");
		}
		if (!prefix.equals("xml") && URI.equals(org.jdom.Namespace.XML_NAMESPACE.getURI())) {
			throw new NamespaceConflictException("invalid prefix in combination with xml namespace");
		}
		try {
			attribute = new org.jdom.Attribute(localName, value, org.jdom.Namespace.getNamespace(prefix, URI));
		}
		catch (org.jdom.IllegalNameException e) {
			throw new IllegalNameException(e, localName);
		}
	}

	@API(value = {Kind.DATA, Kind.TYPE}, 
			doc = {"XOM allows colons in names", 
			"JDOM::Namespace", 
			"XOM::Attribute.Type vs. int"})
	@MapsTo("org.jdom.Attribute(String,String,int,org.jdom.Namespace)")
	public Attribute(String name, String URI, String value, Attribute.Type type) {
		int index = name.indexOf(":");
		String prefix = "";
		String localName = name;
		if (index != -1) {
			prefix = name.substring(0, index);
			localName = name.substring(index, name.length());
		}
		attribute = new org.jdom.Attribute(localName, value, type.type, 
				org.jdom.Namespace.getNamespace(prefix, URI));	
	}
    
    
	
	@Override 
	@API(value = Kind.IDIOM, doc = "XOM::copy vs. JDOM::clone")
	@MapsTo("org.jdom.Attribute#clone()")
	public Node copy() {
		return new Attribute((org.jdom.Attribute)attribute.clone());
	}

	@Override
	@API(value = Kind.HIER, doc = "XOM::Attribute extends Node")
	@MapsTo("")
	public Node getChild(int position) {
		throw new IndexOutOfBoundsException("attributes have children");
	}

	@Override
	@MapsTo("")
	@API(value = Kind.HIER, doc = "XOM::Attribute extends Node")
	public int getChildCount() {
		return 0;
	}

	@API(value = Kind.NAMING, doc = "XOM::getLocalName vs JDOM::getName")
	@MapsTo("org.jdom.Attribute#getName()")
	public String getLocalName() {
		return attribute.getName();
	}

	@API(Kind.EQUAL)
	@MapsTo("org.jdom.Attribute#getNamespacePrefix()")
	public String getNamespacePrefix() {
		return attribute.getNamespacePrefix();
	}

	@API(Kind.EQUAL)
	@MapsTo("org.jdom.Attribute#getNamespaceURI()")
	public String getNamespaceURI() {
		return attribute.getNamespaceURI();
	}

	@API(Kind.EQUAL)
	@MapsTo("org.jdom.Attribute#getQualifiedName()")
	public String getQualifiedName() {
		return attribute.getQualifiedName();
	}

	@API(value = Kind.TYPE, doc = "XOM::Attribute.Type vs int")
	@MapsTo("org.jdom.Attribute#getAttributeType()")
	public Type getType() {
		return new Type(attribute.getAttributeType());
	}

	@Override
	@API(Kind.EQUAL)
	@MapsTo("org.jdom.Attribute#getValue()")
	public String getValue() {
		return attribute.getValue();
	}

	@API(value = Kind.NAMING, doc = "XOM::setLocalName vs. setName")
	@MapsTo("org.jdom.Attribute#setName()")
	public void setLocalName(String localName) {
		try {
			attribute.setName(localName);
		}
		catch (org.jdom.IllegalNameException e) {
			throw new IllegalNameException(e, localName);
		}
	}

	@API(value = {Kind.TYPE, Kind.THROWS}, doc = {"JDOM::Namespace", "JDOM does not check invalid uris"})
	@MapsTo("org.jdom.Attribute#setNamespace(Namespace)")
	public void setNamespace(String prefix, String uri) {
		try {
			attribute.setNamespace(org.jdom.Namespace.getNamespace(prefix, uri));
		}
		catch (org.jdom.IllegalNameException e) {
			throw new NamespaceConflictException(e, uri);
		}
	}

	@API(value = {Kind.NAMING, Kind.TYPE, Kind.THROWS}, doc = {"XOM::setType vs JDOM::setAttributeType", 
			"XOM::Attribute.Type", "XOM::setType throws on null"})
	@MapsTo("org.jdom.Attribute#setAttributeType(int)")
	public void setType(Type type) {
		if (type == null) {
			throw new NullPointerException("type must not be null");
		}
		attribute.setAttributeType(((Type)type).type);
	}

	@API(Kind.EQUAL)
	@MapsTo("org.jdom.Attribute#setValue()")
	public void setValue(String value) {
		try {
			attribute.setValue(value);
		}
		catch (org.jdom.IllegalDataException e) {
			throw new IllegalDataException(e, value);
		}
	}

	@Override
	@API(value = Kind.CORE, doc = "JDOM does not support Attributes to be serialized to XML")
	@MapsTo("")
	public String toXML() {
		return attribute.getName() + "=\"" 
		+ new org.jdom.output.XMLOutputter().escapeAttributeEntities(attribute.getValue()) + "\"";
	}

	@Override
	@API(value = Kind.RESULT, doc = "JDOM::detach returns the detached node")
	@MapsTo("org.jdom.Attribute#detach()")
	public void detach() {
		attribute.detach();
	}

	@API(value = Kind.MACRO, doc = "getBaseURI can be implemented by delegating to parent")
	@MapsTo("")
	public String getBaseURI() {
		org.jdom.Element parent = attribute.getParent();
		return new Element(parent).getBaseURI();
	}

	@Override
	@API(Kind.EQUAL)
	@MapsTo("org.jdom.Attribute#getDocument()")
	public Document getDocument() {
		return new Document(attribute.getDocument());
	}

	@Override
	@API(value = Kind.HIER, doc = "JDOM::Attribute.getParent returns Element, in XOM it's ParentNode")
	@MapsTo("org.jdom.Attribute#getParent()")
	public ParentNode getParent() {
		org.jdom.Element parent = attribute.getParent();
		if (parent == null) {
			return null;
		}
		return new Element(attribute.getParent());
	}

	@Override
	@API(value = Kind.MACRO, doc = "query is not directly supported")
	@MapsTo("")
	public Nodes query(String query, XPathContext namespaces) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			for (Object o: namespaces.namespaces) {
				xpath.addNamespace((org.jdom.Namespace)o);
			}
			List list = xpath.selectNodes(this.attribute);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}
	}

	@Override
	@API(value = Kind.MACRO, doc = "query is not directly supported")
	@MapsTo("")
	public Nodes query(String query) {
		try {
			org.jdom.xpath.XPath xpath = org.jdom.xpath.XPath.newInstance(query);
			List list = xpath.selectNodes(this.attribute);
			return new Nodes(list);
		}
		catch (org.jdom.JDOMException e) {
			throw new XPathException(e.getMessage(), e.getCause());
		}	
	}
	
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof Attribute)) {
			return false;
		}
		return attribute.equals(((Attribute)o).attribute);
	}
	
	@Override
	public int hashCode() {
		return attribute.hashCode();
	}
	
}
