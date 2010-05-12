<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="html" encoding="UTF-8" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"/>
	<xsl:param name="date"/>
	<xsl:template match="/zoo">
		<html xmlns="http://www.w3.org/1999/xhtml" xmlns:xhtml="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
			<head>
				<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
				<title>
				Software Language Processing Suite &#x2014; Grammar Zoo
			</title>
				<link href="zoo.css" rel="stylesheet" type="text/css"/>
			</head>
			<body>
				<h1>
				Software Language Processing Suite
				<br/>
				Grammar Zoo
				<br/>
				<a href="#{language[1]/handle}"><xsl:value-of select="language[1]/name"/></a>
				<xsl:for-each select="language[position()&gt;1]"><xsl:text> &#x2014; </xsl:text><a href="#{handle}"><xsl:value-of select="name"/></a></xsl:for-each>
			</h1>
				<xsl:for-each select="language">
					<hr/>
					<h2>
						<a name="{handle}"/>
						<xsl:value-of select="name"/>
						<br/>
						<a href="#{version[1]/name}">
							<xsl:value-of select="version[1]/name"/>
						</a>
						<xsl:for-each select="version[position()&gt;1]">
							<xsl:text> &#x2014; </xsl:text>
							<a href="#{name}">
								<xsl:value-of select="name"/>
							</a>
						</xsl:for-each>
					</h2>
					<xsl:for-each select="version">
						<h3>
							<a name="{name}"/>
							<xsl:value-of select="name"/>
						</h3>
						<ul>
							<li>
								<xsl:text>Source: </xsl:text>
								<xsl:copy-of select="source/node()"/>
								<xsl:if test="date">
									<xsl:text> (</xsl:text>
									<xsl:value-of select="date"/>
									<xsl:text>)</xsl:text>
								</xsl:if>
								<xsl:if test="specific">
									<xsl:text>, </xsl:text>
									<xsl:value-of select="specific"/>
								</xsl:if>
								<span class="links">
									<xsl:for-each select="link">
									[<a href="{uri}"><xsl:value-of select="name"/></a>]
								</xsl:for-each>
								</span>
							</li>
							<xsl:for-each select="toolset">
								<li>
									<xsl:copy-of select="name/node()"/>
									<xsl:text> tools: </xsl:text>
									<span class="links">
										<xsl:for-each select="link">
											[<a href="{uri}"><xsl:value-of select="name"/></a>]
										</xsl:for-each>
									</span>
								</li>
							</xsl:for-each>
							<xsl:for-each select="grammar">
								<li>
									<xsl:value-of select="name"/>
									<xsl:text> grammar: </xsl:text>
									<ul>
										<li>
											<xsl:text>Browsable:</xsl:text>
											<span class="links">
												[<a href="{../../handle}/{handle}.html">Grammar</a>]
												[<a href="http://www.w3.org/TR/xhtml1/">W3C XHTML Rec</a>]
											</span>
										</li>
										<li>
											<xsl:text>BNF-like Grammar Format:</xsl:text>
											<span class="links">
												[<a href="{../../handle}/{handle}.bgf">Grammar</a>]
												[<a href="http://slps.svn.sourceforge.net/viewvc/slps/shared/xsd/bgf.xsd">Schema</a>]
												[<a href="http://www.w3.org/TR/xml/">W3C XML Rec</a>]
											</span>
										</li>
										<li>
											<xsl:text>Extended Backus-Naur Form:</xsl:text>
											<span class="links">
												[<a href="{../../handle}/{handle}.bnf">Grammar</a>]
												[<a href="http://slps.svn.sourceforge.net/viewvc/slps/shared/xsl/bgf2bnf.xslt?view=markup">BGF&#x21D2;BNF</a>]
											</span>
										</li>
										<xsl:if test="not(../../handle='java')">
											<li>
												<xsl:text>Grammar Deployment Kit:</xsl:text>
												<span class="links">
												[<a href="{../../handle}/{handle}.lll">Grammar</a>]
												[<a href="http://gdk.sourceforge.net/">GDK</a>]
												[<a href="http://gdk.sourceforge.net/gdkref.pdf">LLL Reference</a>]
											</span>
											</li>
										</xsl:if>
										<li>
											<xsl:text>DMS Software Reengineering Toolkit:</xsl:text>
											<span class="links">
												[<a href="{../../handle}/{handle}.dms">Grammar</a>]
												[<a href="http://slps.svn.sourceforge.net/viewvc/slps/shared/xsl/bgf2dms.xslt?view=markup">BGF&#x21D2;DMS</a>]
												[<a href="http://www.semdesigns.com/Products/DMS/DMSToolkit.html">DMS</a>]
											</span>
										</li>
										<li>
											<xsl:text>Syntax Definition Formalism:</xsl:text>
											<span class="links">
												[<a href="{../../handle}/{handle}.sdf">Grammar</a>]
												[<a href="http://slps.svn.sourceforge.net/viewvc/slps/shared/xsl/bgf2sdf.xslt?view=markup">BGF&#x21D2;SDF</a>]
												[<a href="ftp://ftp.stratego-language.org/pub/stratego/docs/sdfintro.pdf">SDF Introduction</a>]
												[<a href="http://dx.doi.org/10.1145/71605.71607">SDF Manual</a>]
												[<a href="http://en.wikipedia.org/wiki/Syntax_Definition_Formalism">Wikipedia</a>]
												[<a href="http://www.meta-environment.org/">MetaEnv</a>]
											</span>
										</li>
									</ul>
								</li>
							</xsl:for-each>
							<xsl:for-each select="item">
								<li>
									<xsl:copy-of select="name/node()"/>
									<span class="links">
										<xsl:for-each select="link">
											[<a href="{uri}"><xsl:value-of select="name"/></a>]
										</xsl:for-each>
									</span>
								</li>
							</xsl:for-each>
						</ul>
					</xsl:for-each>
				</xsl:for-each>
<!--
			Versions of Java programming language are:

			1) "Java 1" = "JDK 1.0" - 1996, standardised by The Java Language Specification
				see http://java.sun.com/docs/books/jls/first_edition/html/index.html
				see http://java.sun.com/docs/books/jls/download/langspec-1.0.pdf
				see jls1 directory for a grammar extractor from §19 of JLS

			2) "Java 1.1" = "JDK 1.1" - 1997, changes covered in The Java Programming Language by Ken Arnold and James Gosling, Addison-Wesley, 1996, ISBN 0-201-63455-4.
				see http://java.sun.com/docs/books/jls/first_edition/html/1.1Update.html (informal)

			3) "Java 2" = "J2SE 1.2" = "Playground" - 1998, standardised by The Java Language Specification Second Edition
				see http://java.sun.com/docs/books/jls/second_edition/html/j.title.doc.html
				see http://java.sun.com/docs/books/jls/download/langspec-2.0.pdf
				see http://java.sun.com/docs/books/jls/clarifications-2-2nd-ed.html
				see jls2 directory for a grammar extractor from §18 of JLS2

			4) "Java 3" = "J2SE 1.3" = "Kestrel", 2000, not standardised due to few language changes
				see http://java.sun.com/j2se/1.3/docs/relnotes/features.html

			5) "Java 4" = "J2SE 1.4" = "Merlin", 2002, not standardised
				see http://java.sun.com/j2se/1.4.2/docs/relnotes/features.html

			6) "Java 5" = "J2SE 5.0" = "Tiger", 2004, standardised by The Java Language Specification Third Edition
				see http://java.sun.com/docs/books/jls/third_edition/html/j3TOC.html
				see http://java.sun.com/docs/books/jls/download/langspec-3.0.pdf
				see http://java.sun.com/j2se/1.5/docs/relnotes/features.html
				see jls3 directory for a grammar extractor from §18 of JLS3

			7) "Java 6" = "Java SE 6" = "Mustang", 2006, not standardised
				see http://java.sun.com/javase/6/features.jsp
				see http://landonf.bikemonkey.org/static/soylatte/

			8) "Java 7" = "Java SE 7" = "Dolphin", under current (2008) development
				see https://jdk7.dev.java.net/
			-->
				<hr xmlns=""/>
				<div xmlns="" class="last">
					<span class="links">[<a href="/">SLPS</a>]</span>
					<em>
						<xsl:text>Last updated: </xsl:text>
						<xsl:value-of select="$date"/>
					</em>
					<br/>
					<a href="http://validator.w3.org/check/referer">
						<img src="valid-xhtml10.png" alt="XHTML 1.0"/>
					</a>
					<a href="http://jigsaw.w3.org/css-validator/check/referer">
						<img src="vcss.png" alt="CSS 2.1"/>
					</a>
				</div>
			</body>
		</html>
	</xsl:template>
</xsl:stylesheet>
