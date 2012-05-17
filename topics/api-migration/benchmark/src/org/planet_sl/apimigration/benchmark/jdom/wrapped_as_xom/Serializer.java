package org.planet_sl.apimigration.benchmark.jdom.wrapped_as_xom;

import org.planet_sl.apimigration.benchmark.anno.MapsTo;
import org.planet_sl.apimigration.benchmark.anno.Progress;
import org.planet_sl.apimigration.benchmark.anno.Wrapping;
import org.planet_sl.apimigration.benchmark.anno.Progress.Status;
import org.planet_sl.apimigration.benchmark.anno.Solution;
import org.planet_sl.apimigration.benchmark.anno.Solution.Strategy;
import org.planet_sl.apimigration.benchmark.anno.Issue;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

@MapsTo("org.jdom.output.XMLOutputter")
public class Serializer {
	@Wrapping
	final Wrap wrapped;

	private static class Wrap {
		org.jdom.output.XMLOutputter outputter;
		OutputStream stream;
		Wrap(org.jdom.output.XMLOutputter outputter, OutputStream stream) {
			this.outputter = outputter;
			this.stream = stream;
		}
	}
	
	
	
	@Wrapping
	private Serializer(Wrap wrapped) {
		this.wrapped = wrapped;
	}
	
	@Progress(value = Status.OK, comment = "")
	@Solution(value = Strategy.MACRO, comment = "")
	@MapsTo("org.jdom.output.XMLOutputter(java.io.OutputStream)")
	public Serializer(OutputStream out) {
		this(new Wrap(new org.jdom.output.XMLOutputter(), out));
	}

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public Serializer(ByteArrayOutputStream out, String encoding) {
		this(out);
	}

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public void flush() throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#getEncoding()
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public String getEncoding() {
		return null;
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#getIndent()
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public int getIndent() {
		// TODO Auto-generated method stub
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#getLineSeparator()
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public String getLineSeparator() {
		return null;
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#getMaxLength()
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public int getMaxLength() {
		return 0;
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#getPreserveBaseURI()
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public boolean getPreserveBaseURI() {
		return false;
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#getUnicodeNormalizationFormC()
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public boolean getUnicodeNormalizationFormC() {
		return false;
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#setIndent(int)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public void setIndent(int indent) {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#setLineSeparator(java.lang.String)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public void setLineSeparator(String lineSeparator) {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#setMaxLength(int)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public void setMaxLength(int maxLength) {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#setOutputStream(java.io.OutputStream)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public void setOutputStream(OutputStream out) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#setPreserveBaseURI(boolean)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public void setPreserveBaseURI(boolean preserve) {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#setUnicodeNormalizationFormC(boolean)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public void setUnicodeNormalizationFormC(boolean normalize) {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#write(nu.xom.Attribute)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void write(Attribute attribute) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#write(nu.xom.Comment)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void write(Comment comment) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#write(nu.xom.DocType)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void write(DocType doctype) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#write(nu.xom.Document)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public void write(Document arg0) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#write(nu.xom.Element)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void write(Element arg0) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#write(nu.xom.ProcessingInstruction)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void write(ProcessingInstruction instruction) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#write(nu.xom.Text)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void write(Text arg0) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#writeAttributes(nu.xom.Element)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void writeAttributes(Element arg0) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#writeChild(nu.xom.Node)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void writeChild(Node node) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#writeEmptyElementTag(nu.xom.Element)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void writeEmptyElementTag(Element element) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#writeEndTag(nu.xom.Element)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void writeEndTag(Element element) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#writeNamespaceDeclaration(java.lang.String,
	 * java.lang.String)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void writeNamespaceDeclaration(String prefix, String uri)
			throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#writeNamespaceDeclarations(nu.xom.Element)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void writeNamespaceDeclarations(Element arg0) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#writeStartTag(nu.xom.Element)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void writeStartTag(Element element) throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see nu.xom.Serializer#writeXMLDeclaration()
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void writeXMLDeclaration() throws IOException {
		// TODO Auto-generated method stub
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#clone()
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected Object clone() throws CloneNotSupportedException {
		// TODO Auto-generated method stub
		return super.clone();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public boolean equals(Object obj) {
		// TODO Auto-generated method stub
		return super.equals(obj);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#finalize()
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	protected void finalize() throws Throwable {
		// TODO Auto-generated method stub
		super.finalize();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public int hashCode() {
		// TODO Auto-generated method stub
		return super.hashCode();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */

	@Progress(value = Status.TODO, comment = "")
	@Solution(value = Strategy.OTHER, comment = "")
	@Issue.Pre("")
	@Issue.Post("")
	@Issue.Throws("")
	public String toString() {
		// TODO Auto-generated method stub
		return super.toString();
	}

}