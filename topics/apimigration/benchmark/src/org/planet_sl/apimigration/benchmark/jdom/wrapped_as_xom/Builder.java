package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Wrapping;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;
import org.planet_sl.apimigration.benchmark.anno.Issue;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;

public class Builder {

	@Wrapping
	org.jdom.input.SAXBuilder builder;

	@Wrapping
	Builder(org.jdom.input.SAXBuilder builder) {
		this.builder = builder;
	}

	// XOM api starts below

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.input.SAXBuilder()")
	public Builder() {
		this(new org.jdom.input.SAXBuilder());
	}

	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.DELEGATE, comment = "")
	@MapsTo("org.jdom.input.SAXBuilder(boolean)")
	public Builder(boolean validate) {
		this(new org.jdom.input.SAXBuilder(validate));
	}

	@Progress(value = Status.GIVENUP, comment = "")
	@Issue.Pre("NodeFactory is not available in JDOM and cannot be used by SAXBuilder")
	@MapsTo("")
	public Builder(boolean validate, NodeFactory factory) {
		throw new UnsupportedOperationException(
				"custom factories not support for JDOM builders");
	}

	@Progress(value = Status.GIVENUP, comment = "")
	@Issue.Pre("NodeFactory is not available in JDOM and cannot be used by SAXBuilder")
	@MapsTo("")
	public Builder(NodeFactory factory) {
		throw new UnsupportedOperationException(
				"custom factories not support for JDOM builders");
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre("Not sure if XMLReader.class can function as a SAX driver")
	@MapsTo("org.jdom.input.SAXBuilder(String)")
	public Builder(org.xml.sax.XMLReader parser) {
		// TODO: don't know about this one...
		this(new org.jdom.input.SAXBuilder(parser.getClass().getName()));
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Pre("Not sure if XMLReader.class can function as a SAX driver")
	@MapsTo("org.jdom.input.SAXBuilder(String,boolean)")
	public Builder(org.xml.sax.XMLReader parser, boolean validate) {
		this(new org.jdom.input.SAXBuilder(parser.getClass().getName(),
				validate));
	}

	@Progress(value = Status.GIVENUP, comment = "")
	@Issue.Pre("NodeFactory is not available in JDOM and cannot be used by SAXBuilder")
	@MapsTo("")
	public Builder(org.xml.sax.XMLReader parser, boolean validate,
			NodeFactory factory) {
		throw new UnsupportedOperationException(
				"custom factories not support for JDOM builders");
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Throws("JDOMException can be thrown without being a JDOMParseException")
	@MapsTo("org.jdom.input.SAXBuilder#build(String)")
	public Document build(String systemID) throws ParsingException, IOException {
		try {
			return new Document(builder.build(systemID));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause(), systemID);
		} catch (org.jdom.JDOMException e) {
			// Don't know if this completely accurate...
			throw new ParsingException(e.getMessage(), e, systemID);
		}
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Throws("JDOMException can be thrown without being a JDOMParseException")
	@MapsTo("org.jdom.input.SAXBuilder#build(InputStream)")
	public Document build(InputStream in) throws ParsingException, IOException {
		try {
			return new Document(builder.build(in));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause());
		} catch (org.jdom.JDOMException e) {
			// Don't know if this completely accurate...
			throw new ParsingException(e.getMessage(), e);
		}
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Throws("JDOMException can be thrown without being a JDOMParseException")
	@MapsTo("org.jdom.input.SAXBuilder#build(InputStream,String)")
	public Document build(InputStream in, String baseURI)
			throws ParsingException, IOException {
		try {
			// NOTE: in SAXBuilder the second arg here is called systemId
			return new Document(builder.build(in, baseURI));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause(), baseURI);
		} catch (org.jdom.JDOMException e) {
			throw new ParsingException(e.getMessage(), e, baseURI);
		}
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Throws("JDOMException can be thrown without being a JDOMParseException")
	@MapsTo("org.jdom.input.SAXBuilder#build(File)")
	public Document build(File in) throws ParsingException, IOException {
		try {
			return new Document(builder.build(in));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause());
		} catch (org.jdom.JDOMException e) {
			// Don't know if this completely accurate...
			throw new ParsingException(e.getMessage(), e);
		}
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Throws("JDOMException can be thrown without being a JDOMParseException")
	@MapsTo("org.jdom.input.SAXBuilder#build(Reader)")
	public Document build(Reader in) throws ParsingException, IOException {
		try {
			return new Document(builder.build(in));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause());
		} catch (org.jdom.JDOMException e) {
			// Don't know if this completely accurate...
			throw new ParsingException(e.getMessage(), e);
		}
	}

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Throws("JDOMException can be thrown without being a JDOMParseException")
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

	@Progress(value = Status.NEEDSWORK, comment = "")
	@Solution(value = Strategy.ADVANCED_DELEGATE, comment = "")
	@Issue.Throws("JDOMException can be thrown without being a JDOMParseException")
	@MapsTo("org.jdom.input.SAXBuilder#build(String,String)")
	public Document build(String document, String baseURI)
			throws ParsingException, IOException {
		try {
			return new Document(builder.build(new StringReader(document),
					baseURI));
		} catch (org.jdom.input.JDOMParseException e) {
			throw new ParsingException(e.getMessage(), e.getCause(), baseURI);
		} catch (org.jdom.JDOMException e) {
			// Don't know if this completely accurate...
			throw new ParsingException(e.getMessage(), e, baseURI);
		}
	}

	@Progress(value = Status.GIVENUP, comment = "")
	@Issue.Post("No NodeFactory available since it does not work with SAXBuilder")
	@MapsTo("")
	public NodeFactory getNodeFactory() {
		throw new UnsupportedOperationException(
				"no nodefactories supported in JDOM");
	}

}