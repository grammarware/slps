<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="html" encoding="UTF-8" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"/>
	<xsl:param name="date"/>
	<xsl:template match="/zoo">
		<html xmlns="http://www.w3.org/1999/xhtml" xmlns:xhtml="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
			<head>
				<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
				<title>
				Software Language Processing Suite — Grammar Zoo
			</title>
				<link href="../slps.css" rel="stylesheet" type="text/css"/>
			</head>
			<body style="background-color:#9C9;">
				<h1>
				Software Language Processing Suite
				<br/>
				Grammar Zoo
				</h1>
				<h2><xsl:value-of select="count(//grammar)"/> grammars and counting</h2>
				<h1>
					<a href="#{language[1]/handle}">
						<xsl:value-of select="language[1]/name"/>
					</a>
					<xsl:for-each select="language[position()&gt;1]">
						<xsl:text> — </xsl:text>
						<a href="#{handle}">
							<xsl:value-of select="name"/>
						</a>
					</xsl:for-each>
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
							<xsl:text> — </xsl:text>
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
							<xsl:for-each select="*">
								<xsl:choose>
									<xsl:when test="local-name(.)='name'"/>
									<xsl:when test="local-name(.)='toolset' and @ref">
										<xsl:variable name="name" select="@ref"/>
										<xsl:apply-templates select="/zoo/toolset[@name=$name]"/>
									</xsl:when>
									<xsl:otherwise>
										<xsl:apply-templates select="."/>
									</xsl:otherwise>
								</xsl:choose>
							</xsl:for-each>
						</ul>
					</xsl:for-each>
				</xsl:for-each>
				<hr/>
				<h3>Appendix: Notations and Formats</h3>
				<ul>
					<li>
						<xsl:text>Browsable:</xsl:text>
						<span class="links">
							[<a href="http://www.w3.org/TR/xhtml1/">W3C XHTML Rec</a>]
						</span>
					</li>
					<li>
						<xsl:text>BNF-like Grammar Format:</xsl:text>
						<span class="links">
							[<a href="http://slps.svn.sourceforge.net/viewvc/slps/shared/xsd/bgf.xsd">Schema</a>]
							[<a href="http://www.w3.org/TR/xml/">W3C XML Rec</a>]
							[<a href="http://www.w3.org/TR/xmlschema11-1/">W3C XSD WD 1</a>]
							[<a href="http://www.w3.org/TR/xmlschema11-2/">W3C XSD WD 2</a>]
						</span>
					</li>
					<li>
						<xsl:text>Grammar Deployment Kit:</xsl:text>
						<span class="links">
							[<a href="http://gdk.sourceforge.net/">GDK</a>]
							[<a href="http://gdk.sourceforge.net/gdkref.pdf">LLL Reference</a>]
						</span>
					</li>
					<li>
						<xsl:text>DMS Software Reengineering Toolkit:</xsl:text>
						<span class="links">
							[<a href="http://www.semdesigns.com/Products/DMS/DMSToolkit.html">DMS</a>]
						</span>
					</li>
					<li>
						<xsl:text>Syntax Definition Formalism:</xsl:text>
						<span class="links">
							[<a href="ftp://ftp.stratego-language.org/pub/stratego/docs/sdfintro.pdf">SDF Introduction</a>]
							[<a href="http://dx.doi.org/10.1145/71605.71607">SDF Manual</a>]
							[<a href="http://en.wikipedia.org/wiki/Syntax_Definition_Formalism">Wikipedia</a>]
							[<a href="http://www.meta-environment.org/">MetaEnv</a>]
						</span>
					</li>
					<li>
						<xsl:text>Rascal Meta Programming Language:</xsl:text>
						<span class="links">
							[<a href="http://www.rascal-mpl.org/">Rascal DSL</a>]
						</span>
					</li>
				</ul>
				<hr/>
				<div class="last">
					<span class="links">[<a href="/">↑SLPS</a>]</span>
					<em>
						<xsl:text>The page is maintained by Dr. </xsl:text>
						<a href="http://grammarware.net/">Vadim Zaytsev</a>
						<xsl:text> a.k.a. @</xsl:text>
						<a href="http://twitter.com/grammarware">grammarware</a>
						<xsl:text>. Last updated: </xsl:text>
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
	<xsl:template match="toolset">
		<li xmlns="http://www.w3.org/1999/xhtml">
			<xsl:copy-of select="name/node()"/>
			<xsl:text> tools: </xsl:text>
			<span class="links">
				<xsl:for-each select="link">
					[<a href="{uri}"><xsl:value-of select="name"/></a>]
				</xsl:for-each>
			</span>
		</li>
	</xsl:template>
	<xsl:template match="source">
		<li>
			<xsl:text>Source: </xsl:text>
			<xsl:copy-of select="title/node()"/>
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
	</xsl:template>
	<xsl:template match="grammar">
		<li>
			<xsl:value-of select="name"/>
			<xsl:text> grammar: </xsl:text>
			<span class="links">
				[<a href="{../../handle}/{handle}.html">Browsable</a>]
				[<a href="{../../handle}/{handle}.bgf">BGF</a>]
				[<a href="{../../handle}/{handle}.bnf">EBNF</a>]
				<xsl:if test="not(../../handle='java') and not(../../handle='xpath') and not(handle='iso-23270-2003') and not(handle='iso-23270-2003-recovered')">
					[<a href="{../../handle}/{handle}.lll">LLL</a>]
				</xsl:if>
				[<a href="{../../handle}/{handle}.dms">DMS BNF</a>]
				[<a href="{../../handle}/{handle}.sdf">SDF</a>]
				[<a href="{../../handle}/{handle}.rsc">Rascal</a>]
			</span>
		</li>
	</xsl:template>
	<xsl:template match="item">
		<li>
			<xsl:copy-of select="name/node()"/>
			<span class="links">
				<xsl:for-each select="link">
					[<a href="{uri}"><xsl:value-of select="name"/></a>]
				</xsl:for-each>
			</span>
		</li>
	</xsl:template>
</xsl:stylesheet>
