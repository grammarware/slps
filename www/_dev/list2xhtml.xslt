<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="html" encoding="UTF-8" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"/>
	<xsl:param name="date"/>
	<xsl:template match="/zoo">
		<html xmlns="http://www.w3.org/1999/xhtml" xmlns:xhtml="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
			<head>
				<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
				<title>
					<xsl:text>Software Language Processing Suite — Grammar </xsl:text>
					<xsl:value-of select="name"/>
				</title>
				<link href="../slps.css" rel="stylesheet" type="text/css"/>
				<script type="text/javascript">
					<xsl:text>

				  var _gaq = _gaq || [];
				  _gaq.push(['_setAccount', 'UA-3743366-5']);
				  _gaq.push(['_trackPageview']);

				  (function() {
				    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
				    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
				    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
				  })();
					</xsl:text>
				</script>
			</head>
			<body style="background-color:#9C9;">
				<h1>Software Language Processing Suite</h1>
				<xsl:if test="name='zoo'">
					<h1>
						<xsl:text>Grammar Zoo | </xsl:text>
						<a href="../tank/">Grammar Tank</a>
					</h1>
					<div class="c">
						<em>
							The objective of the Grammar Zoo is to accumulate grammars of contemporary 
							programming languages, extracted and recovered from language documentation, parser
							specifications and other artifacts and make them available in a range of formats.
						</em>
					</div>
				</xsl:if>
				<xsl:if test="name='tank'">
					<h1>
						<a href="../zoo/">Grammar Zoo</a>
						<xsl:text> | Grammar Tank</xsl:text>
					</h1>
					<div class="c">
						<em>
							The objective of the Grammar Tank is to compose a grammar base of multiple different
							grammars for the same intended language to be used in language engineering research.
						</em>
					</div>
				</xsl:if>
				<h2><xsl:value-of select="count(//grammar)"/> grammars and counting</h2>
				<h1>
					<a href="#{translate(language[1]/handle,' +#“”','_ps__')}">
						<xsl:choose>
							<xsl:when test="language[1]/short">
								<xsl:value-of select="language[1]/short"/>
							</xsl:when>
							<xsl:otherwise>
								<xsl:value-of select="language[1]/name"/>
							</xsl:otherwise>
						</xsl:choose>
					</a>
					<xsl:for-each select="language[position()&gt;1]">
						<xsl:text> — </xsl:text>
						<a href="#{translate(handle,' +#“”','_ps__')}">
							<xsl:choose>
								<xsl:when test="short">
									<xsl:value-of select="short"/>
								</xsl:when>
								<xsl:otherwise>
									<xsl:value-of select="name"/>
								</xsl:otherwise>
							</xsl:choose>
						</a>
					</xsl:for-each>
				</h1>
				<xsl:for-each select="language">
					<hr/>
					<h2>
						<a name="{translate(handle,' +#“”','_ps__')}"/>
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
							<a name="{translate(name,' +#“”','_ps__')}"/>
							<xsl:value-of select="name"/>
							<xsl:if test="count(grammar) &gt; 1">
								<em>
									<xsl:text> (</xsl:text>
									<xsl:value-of select="count(grammar)"/>
									<xsl:text> grammars)</xsl:text>
								</em>
							</xsl:if>
						</h3>
						<ul>
							<xsl:for-each select="*">
								<xsl:choose>
									<xsl:when test="local-name(.)='name'"/>
									<xsl:when test="local-name(.)='toolset' and @ref">
										<xsl:variable name="name" select="@ref"/>
										<xsl:apply-templates select="/zoo/toolset[@name=$name]"/>
									</xsl:when>
									<xsl:when test="local-name(.)='item' and @ref">
										<xsl:variable name="name" select="@ref"/>
										<xsl:apply-templates select="/zoo/item[@name=$name]"/>
									</xsl:when>
									<xsl:when test="local-name(.)='itemset' and @ref">
										<xsl:variable name="name" select="@ref"/>
										<xsl:apply-templates select="/zoo/itemset[@name=$name]/item"/>
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
							[<a href="http://slps.svn.sourceforge.net/viewvc/slps/shared/xsd/bgf.xsd?view=markup">Schema</a>]
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
							[<a href="http://www.rascal-mpl.org/">Rascal MPL</a>]
							[<a href="http://www.cwi.nl/research-groups/Software-Analysis-and-Transformation">SWAT</a>]
						</span>
					</li>
				</ul>
				<hr/>
				<div class="last">
					<strong>
						All grammars are distributed on terms of the <a href="http://creativecommons.org/licenses/by/3.0/">CC-BY</a> license
						as well as on terms of any other license bound to the source of our research in a way that enforces
						its propagation to derivatives.<br/>
					</strong>
					<xsl:text>The page is maintained by Dr. </xsl:text>
					<a href="http://grammarware.net/">Vadim Zaytsev</a>
					<xsl:text> a.k.a. @</xsl:text>
					<a href="http://twitter.com/grammarware">grammarware</a>
					<xsl:text>. Last updated: </xsl:text>
					<xsl:value-of select="$date"/>
					<span class="links">[<a href="/">↑SLPS</a>]</span>
					<br/>
					<a href="http://creativecommons.org/licenses/by/3.0/">
						<img src="http://i.creativecommons.org/l/by/3.0/88x31.png" alt="CC-BY"/>
					</a>
					<a href="http://validator.w3.org/check/referer">
						<img src="../img/vxhtml.png" alt="XHTML 1.0"/>
					</a>
					<a href="http://jigsaw.w3.org/css-validator/check/referer">
						<img src="../img/vcss.png" alt="CSS 2.1"/>
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
				<xsl:apply-templates select="link"/>
			</span>
		</li>
	</xsl:template>
	<xsl:template match="link">
		<xsl:text> [</xsl:text>
		<xsl:choose>
			<xsl:when test="uri">
				<a xmlns="http://www.w3.org/1999/xhtml" href="{uri}">
					<xsl:value-of select="name"/>
				</a>
			</xsl:when>
			<xsl:when test="mu">
				<a xmlns="http://www.w3.org/1999/xhtml" href="http://slps.svn.sourceforge.net/viewvc/slps/{mu}?view=markup">
					<xsl:value-of select="name"/>
				</a>
			</xsl:when>
			<xsl:when test="xbgf">
				<a xmlns="http://www.w3.org/1999/xhtml" href="{xbgf}.html">
					<xsl:value-of select="name"/>
					<xsl:text>.xbgf</xsl:text>
				</a>
			</xsl:when>
			<xsl:when test="slps">
				<a xmlns="http://www.w3.org/1999/xhtml" href="http://slps.svn.sourceforge.net/viewvc/slps/{slps}">
					<xsl:value-of select="name"/>
				</a>
			</xsl:when>
			<xsl:when test="doi">
				<a xmlns="http://www.w3.org/1999/xhtml" href="http://dx.doi.org/{doi}">
					<xsl:value-of select="name"/>
				</a>
			</xsl:when>
		</xsl:choose>
		<xsl:text>] </xsl:text>
	</xsl:template>
	<xsl:template match="source">
		<li xmlns="http://www.w3.org/1999/xhtml">
			<strong>Source: </strong>
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
				<xsl:apply-templates select="link"/>
			</span>
		</li>
	</xsl:template>
	<xsl:template match="grammar">
		<li xmlns="http://www.w3.org/1999/xhtml">
			<xsl:value-of select="name"/>
			<xsl:text> grammar: </xsl:text>
			<span class="links">
				[<a href="{../../handle}/{handle}.html" class="red">Browsable</a>]
				[<a href="{../../handle}/{handle}.bgf">BGF</a>]
				[<a href="{../../handle}/{handle}.bnf">EBNF</a>]
				<!--
				<xsl:if test="(../../handle='c') or (../../handle='cpp') or ((../../handle='csharp') and not(handle='iso-23270-2003') and not(handle='iso-23270-2003-recovered'))">
					[<a href="{../../handle}/{handle}.lll">LLL</a>]
				</xsl:if>
				-->
				[<a href="{../../handle}/{handle}.dms">DMS BNF</a>]
				[<a href="{../../handle}/{handle}.sdf">SDF</a>]
				[<a href="{../../handle}/{handle}.rsc">Rascal</a>]
			</span>
		</li>
	</xsl:template>
	<xsl:template match="item">
		<li xmlns="http://www.w3.org/1999/xhtml">
			<xsl:copy-of select="name/node()"/>
			<span class="links">
				<xsl:apply-templates select="link"/>
			</span>
		</li>
	</xsl:template>
</xsl:stylesheet>
