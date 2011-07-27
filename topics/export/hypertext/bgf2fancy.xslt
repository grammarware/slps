<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:import href="bgf2xhtml.xslt"/>
	<xsl:output method="html" encoding="UTF-8" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"/>
	<xsl:param name="date"/>
	<xsl:param name="zoo"/>
	<xsl:param name="id"/>
	<xsl:param name="report"/>
	<xsl:variable name="mysrc" select="document($zoo)//grammar[handle=$id]/source"/>
	<xsl:variable name="mysrcs" select="document($zoo)//grammarset/grammarname[.=$id]/../source"/>
	<xsl:variable name="xmlreport" select="document($report)/xml"/>
	<xsl:template match="/bgf:grammar">
		<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
			<head>
				<title>
					<xsl:text>Browsable </xsl:text>
					<xsl:value-of select="concat($mysrc/../../name,$mysrcs/../../name)"/>
					<xsl:text> Grammar</xsl:text>
				</title>
				<style type="text/css">
		          .label, .sel { color: green; }
		          .marked { background-color: #FFE5B4;}
		          .nt { color: blue; font-weight: bold; }
		          .t { color: red;  font-style:italic; }
		          .meta { color: green; font-style:italic; font-family: Roman, "Times New Roman", serif; }
		          .b { text-align: right; font-style:italic;}
		          .date { font-size: small; }
		          pre
		          {
		          border: 1px solid black;
		          border-spacing: 2px;
		          border-collapse: collapse;
		          background-color: #ECECEC;
		          }
		        </style>
			</head>
			<body>
				<h1>
					<xsl:text>Browsable </xsl:text>
					<xsl:value-of select="concat($mysrc/../../name,$mysrcs/../../name)"/>
					<xsl:text> Grammar</xsl:text>
				</h1>
				<a href="http://creativecommons.org/licenses/by/3.0/">
					<img src="http://i.creativecommons.org/l/by/3.0/88x31.png" alt="CC-BY"/>
				</a>
				<p>
					<xsl:text>Extracted and/or recovered by </xsl:text>
					<strong>
						<a href="http://grammarware.net">Vadim Zaytsev</a>
					</strong>
					<xsl:text>,	see </xsl:text>
					<xsl:choose>
						<xsl:when test="substring-after($zoo,'_dev/')='tank.xml'">
							<a href="http://slps.sourceforge.net/tank/">Grammar Tank</a>
						</xsl:when>
						<xsl:otherwise>
							<a href="http://slps.sourceforge.net/zoo/">Grammar Zoo</a>
						</xsl:otherwise>
					</xsl:choose>
					<xsl:text> for details. </xsl:text>
					<xsl:if test="$mysrc and count($mysrc)=1">
						<br/>
						<xsl:text>Source used for this grammar: </xsl:text>
						<xsl:copy-of select="$mysrc/title/node()"/>
						<xsl:text> (</xsl:text>
						<xsl:value-of select="$mysrc/date"/>
						<xsl:text>) </xsl:text>
						<xsl:value-of select="$mysrc/specific"/>
					</xsl:if>
					<xsl:if test="$mysrc and count($mysrc)!=1">
						<br/>
						<xsl:text>Sources used for this grammar: </xsl:text>
						<xsl:for-each select="$mysrc">
							<xsl:copy-of select="title/node()"/>
							<xsl:text> (</xsl:text>
							<xsl:value-of select="date"/>
							<xsl:text>); </xsl:text>
						</xsl:for-each>
					</xsl:if>
					
					<xsl:if test="$mysrcs">
						<br/>
						<xsl:text>Source used for this grammar: </xsl:text>
						<xsl:copy-of select="$mysrcs/title/node()"/>
						<xsl:text> (</xsl:text>
						<xsl:value-of select="$mysrcs/date"/>
						<xsl:text>) </xsl:text>
						<xsl:value-of select="$mysrcs/specific"/>
					</xsl:if>
				</p>
				<h2>Summary</h2>
				<ul>
					<li>
						<xsl:text>Number of production rules: </xsl:text>
						<strong>
							<xsl:value-of select="count(bgf:production)"/>
						</strong>
					</li>
					<li>
						<xsl:text>Number of top alternatives: </xsl:text>
						<strong>
							<xsl:value-of select="count(bgf:production)-count(*/bgf:expression/choice)+count(bgf:production/bgf:expression/choice/bgf:expression)"/>
						</strong>
					</li>
					<li>
						<xsl:text>Number of defined nonterminal symbols: </xsl:text>
						<strong>
							<xsl:value-of select="count(bgf:production/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())])"/>
						</strong>
					</li>
					<li>
						<xsl:text>Root nonterminal symbols: </xsl:text>
						<xsl:choose>
							<xsl:when test="root">
								<code>
									<a href="#{root[1]}" class="nt">
										<xsl:value-of select="root[1]"/>
									</a>
									<xsl:for-each select="root[position()&gt;1]">
										<xsl:text>, </xsl:text>
										<a href="#{.}" class="nt">
											<xsl:value-of select="."/>
										</a>
									</xsl:for-each>
								</code>
							</xsl:when>
							<xsl:otherwise>—</xsl:otherwise>
						</xsl:choose>
					</li>
					<li>
						<xsl:text>Other top nonterminal symbols: </xsl:text>
						<xsl:choose>
							<xsl:when test="$xmlreport/top">
								<strong>
									<xsl:value-of select="count($xmlreport/top)"/>
								</strong>
								<xsl:text>: </xsl:text>
								<code>
									<a href="#{$xmlreport/top[1]}" class="nt">
										<xsl:value-of select="$xmlreport/top[1]"/>
									</a>
									<xsl:for-each select="$xmlreport/top[position()&gt;1]">
										<xsl:text>, </xsl:text>
										<a href="#{.}" class="nt">
											<xsl:value-of select="."/>
										</a>
									</xsl:for-each>
								</code>
							</xsl:when>
							<xsl:otherwise>—</xsl:otherwise>
						</xsl:choose>
					</li>
					<li>
						<xsl:text>Bottom nonterminal symbols: </xsl:text>
						<xsl:choose>
							<xsl:when test="$xmlreport/bottom">
								<strong>
									<xsl:value-of select="count($xmlreport/bottom)"/>
								</strong>
								<xsl:text>: </xsl:text>
								<code>
									<a href="#{$xmlreport/bottom[1]}" class="nt">
										<xsl:value-of select="$xmlreport/bottom[1]"/>
									</a>
									<xsl:for-each select="$xmlreport/bottom[position()&gt;1]">
										<xsl:text>, </xsl:text>
										<a href="#{.}" class="nt">
											<xsl:value-of select="."/>
										</a>
									</xsl:for-each>
								</code>
							</xsl:when>
							<xsl:otherwise>—</xsl:otherwise>
						</xsl:choose>
					</li>
					<li>
						<xsl:text>Number of used terminal symbols: </xsl:text>
						<strong>
							<xsl:choose>
								<xsl:when test="count($xmlreport/terminal)+count($xmlreport/keyword) &gt; 0">
									<xsl:value-of select="count($xmlreport/terminal)+count($xmlreport/keyword)"/>
								</xsl:when>
								<xsl:otherwise>—</xsl:otherwise>
							</xsl:choose>
						</strong>
					</li>
					<xsl:if test="$xmlreport/terminal">
						<li>
							<xsl:text>Special terminal symbols: </xsl:text>
							<strong>
								<xsl:value-of select="count($xmlreport/terminal)"/>
							</strong>
							<xsl:text>: </xsl:text>
							<code class="t">
								<xsl:text>"</xsl:text>
								<xsl:value-of select="$xmlreport/terminal[1]"/>
								<xsl:text>"</xsl:text>
								<xsl:for-each select="$xmlreport/terminal[position()&gt;1]">
									<xsl:text>, "</xsl:text>
									<xsl:value-of select="."/>
									<xsl:text>"</xsl:text>
								</xsl:for-each>
							</code>
						</li>
					</xsl:if>
					<xsl:if test="$xmlreport/keyword">
						<li>
							<xsl:text>Keywords: </xsl:text>
							<strong>
								<xsl:value-of select="count($xmlreport/keyword)"/>
							</strong>
							<xsl:text>: </xsl:text>
							<code class="t">
								<xsl:text>"</xsl:text>
								<xsl:value-of select="$xmlreport/keyword[1]"/>
								<xsl:text>"</xsl:text>
								<xsl:for-each select="$xmlreport/keyword[position()&gt;1]">
									<xsl:text>, "</xsl:text>
									<xsl:value-of select="."/>
									<xsl:text>"</xsl:text>
								</xsl:for-each>
							</code>
						</li>
					</xsl:if>
				</ul>
				<h2>Syntax</h2>
				<xsl:for-each select="./bgf:*">
					<pre>
						<xsl:apply-templates select="."/>
					</pre>
				</xsl:for-each>
				<hr/>
				<div class="b">
					Maintained by Dr. <a href="http://grammarware.net/">Vadim Zaytsev</a> a.k.a. @<a href="http://twitter.com/grammarware">grammarware</a>.
					Last updated: <xsl:value-of select="$date"/>.
				</div>
			</body>
		</html>
	</xsl:template>
</xsl:stylesheet>
