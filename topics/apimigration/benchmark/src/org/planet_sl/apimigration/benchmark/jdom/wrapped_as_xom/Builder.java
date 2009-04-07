package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;


public class Builder  {

	org.jdom.input.SAXBuilder builder;
	
	Builder(org.jdom.input.SAXBuilder builder) {
		this.builder = builder;
	}
	
	// XOM api starts below
	
	@MapsTo("org.jdom.input.SAXBuilder()")
	public Builder() {
		this(new org.jdom.input.SAXBuilder());
	}
	
	@MapsTo("org.jdom.input.SAXBuilder(boolean)")
	public Builder(boolean validate) {
		this(new org.jdom.input.SAXBuilder(validate));
	}
	
	@MapsTo("")
	public Builder(boolean validate, NodeFactory factory) {
		throw new UnsupportedOperationException("custom factories not support for JDOM builders"); 
	}
    
	@MapsTo("")
	public Builder(NodeFactory factory) {
		throw new UnsupportedOperationException("custom factories not support for JDOM builders");
	}
    
	@MapsTo("org.jdom.input.SAXBuilder(String)")
	public Builder(org.xml.sax.XMLReader parser) {
		// TODO: don't know about this one...
		this(new org.jdom.input.SAXBuilder(parser.getClass().getName()));
	}

	@MapsTo("org.jdom.input.SAXBuilder(String,boolean)")
	public Builder(org.xml.sax.XMLReader parser, boolean validate) {
		this(new org.jdom.input.SAXBuilder(parser.getClass().getName(), validate));
	}

	@MapsTo("")
	public Builder(org.xml.sax.XMLReader parser, boolean validate, NodeFactory factory) {
		throw new UnsupportedOperationException("custom factories not support for JDOM builders");
	}
	
	@MapsTo("org.jdom.input.SAXBuilder#build(String)")
	public Document build(String systemID) throws ParsingException,
			IOException {
		try {
			return new Document(builder.build(systemID));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause(), systemID);
		} catch (org.jdom.JDOMException e) {
			// Don't know if this completely accurate...
			throw new ParsingException(e.getMessage(), e, systemID);
		}
	}

	@MapsTo("org.jdom.input.SAXBuilder#build(InputStream)")
	public Document build(InputStream in) throws ParsingException,
			IOException {
		try {
			return new Document(builder.build(in));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause());
		} catch (org.jdom.JDOMException e) {
			// Don't know if this completely accurate...
			throw new ParsingException(e.getMessage(), e);
		}
	}

	@MapsTo("org.jdom.input.SAXBuilder#build(InputStream,String)")
	public Document build(InputStream in, String baseURI)
			throws ParsingException, IOException {
		try {
			// NOTE: in SAXBuilder the second arg here is called systemId
			return new Document(builder.build(in, baseURI));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause(), baseURI);
		}
		catch (org.jdom.JDOMException e) {
			throw new ParsingException(e.getMessage(), e, baseURI);
		}
	}

	@MapsTo("org.jdom.input.SAXBuilder#build(File)")
	public Document build(File in) throws ParsingException,
			IOException {
		try {
			return new Document(builder.build(in));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause());
		} catch (org.jdom.JDOMException e) {
			// Don't know if this completely accurate...
			throw new ParsingException(e.getMessage(), e);
		}
	}

	@MapsTo("org.jdom.input.SAXBuilder#build(Reader)")
	public Document build(Reader in) throws ParsingException,
			IOException {
		try {
			return new Document(builder.build(in));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause());
		} catch (org.jdom.JDOMException e) {
			// Don't know if this completely accurate...
			throw new ParsingException(e.getMessage(), e);
		}
	}

	@MapsTo("org.jdom.input.SAXBuilder#build(Reader,String)")
	public Document build(Reader in, String baseURI) throws ParsingException,
			 IOException {
		try {
			return new Document(builder.build(in, baseURI));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause(), baseURI);
		} catch (org.jdom.JDOMException e) {
			// Don't know if this completely accurate...
			throw new ParsingException(e.getMessage(), e, baseURI);
		}
	}

	@MapsTo("org.jdom.input.SAXBuilder#build(String,String)")
	public Document build(String document, String baseURI)
			throws ParsingException, IOException {
		try {
			return new Document(builder.build(new StringReader(document), baseURI));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause(), baseURI);
		} catch (org.jdom.JDOMException e) {
			// Don't know if this completely accurate...
			throw new ParsingException(e.getMessage(), e, baseURI);
		}
	}

	@MapsTo("")
	public NodeFactory getNodeFactory() {
		throw new UnsupportedOperationException("no nodefactories supported in JDOM");
	}

}
