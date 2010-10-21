<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:ldf="http://planet-sl.org/ldf" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:param name="grammar"/>
	<xsl:variable name="bgf" select="document($grammar)/bgf:grammar"/>
	<xsl:template match="/spec">
		<ldf:document xmlns:ldf="http://planet-sl.org/ldf">
			<metadata>
				<xsl:if test="header/w3c-doctype">
					<body>w3c</body>
					<number>
						<xsl:value-of select="header/w3c-designation"/>
					</number>
				</xsl:if>
				<xsl:for-each select="header/authlist/author">
					<author>
						<name>
							<xsl:value-of select="name"/>
						</name>
						<xsl:if test="affiliation">
							<affiliation>
								<xsl:value-of select="affiliation"/>
							</affiliation>
						</xsl:if>
						<xsl:if test="email">
							<email>
								<xsl:value-of select="email"/>
							</email>
						</xsl:if>
					</author>
				</xsl:for-each>
				<topic>
					<xsl:value-of select="header/title"/>
				</topic>
				<xsl:choose>
					<xsl:when test="header/w3c-doctype = 'W3C Recommendation'">
						<status>approved</status>
					</xsl:when>
					<xsl:otherwise>
						<status>unknown</status>
					</xsl:otherwise>
				</xsl:choose>
				<xsl:choose>
					<xsl:when test="header/version = 'Version 1.0'">
						<version>1.0</version>
					</xsl:when>
					<xsl:when test="header/version = 'Version 2.0'">
						<version>2.0</version>
					</xsl:when>
					<xsl:otherwise>
						<version>
							<xsl:value-of select="header/version"/>
						</version>
					</xsl:otherwise>
				</xsl:choose>
				<xsl:for-each select="header/prevlocs/loc">
					<previous>
						<title>
							<xsl:choose>
								<xsl:when test="substring(text(),1,7) = 'http://'">
									<xsl:value-of select="substring-after(substring-after(substring-after(text(),'http://www.w3.org/'),'/'),'/')"/>
								</xsl:when>
								<xsl:otherwise>
									<xsl:value-of select="text()"/>
								</xsl:otherwise>
							</xsl:choose>
						</title>
						<uri>
							<xsl:value-of select="@href"/>
						</uri>
					</previous>
				</xsl:for-each>
				<date>
					<xsl:value-of select="header/pubdate/day"/>
					<xsl:text> </xsl:text>
					<xsl:value-of select="header/pubdate/month"/>
					<xsl:text> </xsl:text>
					<xsl:value-of select="header/pubdate/year"/>
				</date>
			</metadata>
			<part>
				<metadata>
					<role>front-matter</role>
				</metadata>
				<section>
					<metadata>
						<role>abstract</role>
						<title>Abstract</title>
					</metadata>
					<content>
						<xsl:for-each select="header/abstract/p">
							<para>
								<xsl:apply-templates select="node()"/>
							</para>
						</xsl:for-each>
					</content>
				</section>
				<section>
					<metadata>
						<role>scope</role>
						<title>Status of this document</title>
<!-- TODO: should be added later with improving transformations. The information is useful but not automatically extractable -->
					</metadata>
					<content>
						<xsl:for-each select="header/status/p">
							<para>
								<xsl:apply-templates select="node()"/>
							</para>
						</xsl:for-each>
					</content>
				</section>
<!-- skipped: langusage, revisiondesc -->
				<section>
					<metadata>
						<role>list-of-contents</role>
						<title>Table of contents</title>
					</metadata>
					<placeholder/>
<!-- TODO: should be added later with improving transformations. The information is useful but not automatically extractable -->
				</section>
			</part>
			<xsl:if test="body">
				<part>
					<metadata>
						<role>core-part</role>
					</metadata>
					<xsl:apply-templates select="body/div1"/>
				</part>
			</xsl:if>
			<xsl:if test="back">
				<part>
					<metadata>
						<role>back-matter</role>
					</metadata>
					<xsl:apply-templates select="back/div1|back/inform-div1"/>
				</part>
			</xsl:if>
		</ldf:document>
	</xsl:template>
	<xsl:template match="div1|inform-div1">
		<xsl:choose>
			<xsl:when test="head = 'Introduction' or head = 'Conformance'">
				<section>
					<metadata>
						<xsl:if test="@id">
							<id>
								<xsl:value-of select="@id"/>
							</id>
						</xsl:if>
						<xsl:choose>
							<xsl:when test="head = 'Introduction'">
								<role>foreword</role>
							</xsl:when>
							<xsl:when test="head = 'Conformance'">
								<role>conformance</role>
							</xsl:when>
						</xsl:choose>
						<title>
							<xsl:value-of select="head"/>
						</title>
					</metadata>
					<content>
						<xsl:apply-templates select="*[local-name() != 'head']"/>
					</content>
				</section>
			</xsl:when>
			<xsl:when test="head = 'References'">
				<section>
					<metadata>
						<xsl:if test="@id">
							<id>
								<xsl:value-of select="@id"/>
							</id>
						</xsl:if>
						<role>references</role>
						<xsl:if test="local-name() = 'inform-div1'">
							<type>informative</type>
						</xsl:if>
						<title>
							<xsl:value-of select="head"/>
						</title>
					</metadata>
					<xsl:apply-templates select="div2"/>
				</section>
			</xsl:when>
			<xsl:otherwise>
				<section>
					<metadata>
						<xsl:if test="@id">
							<id>
								<xsl:value-of select="@id"/>
							</id>
						</xsl:if>
						<role>top-section</role>
						<title>
							<xsl:value-of select="head"/>
						</title>
					</metadata>
					<xsl:if test="p|ulist|slist|blist">
<!-- if there is pure content, not just subsections, then create a special section for it -->
<!-- TODO: any other variations, beside head|scrap|div2|p|ulist? -->
						<subsection>
							<metadata>
								<role>description</role>
								<xsl:choose>
									<xsl:when test="local-name() = 'inform-div1'">
										<type>informative</type>
									</xsl:when>
									<xsl:otherwise>
										<type>normative</type>
									</xsl:otherwise>
								</xsl:choose>
							</metadata>
							<content>
								<xsl:apply-templates select="p|ulist|slist|blist"/>
							</content>
						</subsection>
					</xsl:if>
					<xsl:apply-templates select="scrap"/>
					<xsl:apply-templates select="div2"/>
				</section>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="div2|div3|div4|div5">
		<subsection>
			<metadata>
				<xsl:if test="@id">
					<id>
						<xsl:value-of select="@id"/>
					</id>
				</xsl:if>
				<role>subtopic</role>
				<title>
					<xsl:value-of select="head"/>
				</title>
			</metadata>
			<xsl:if test="p|ulist|slist|blist">
<!-- if there is pure content, not just subsections, then create a special section for it -->
<!-- TODO: any other variations, beside head|scrap|div2|p|ulist? -->
				<subsection>
					<metadata>
						<role>description</role>
						<xsl:choose>
							<xsl:when test="local-name(..) = 'inform-div1'">
								<type>informative</type>
							</xsl:when>
							<xsl:otherwise>
								<type>normative</type>
							</xsl:otherwise>
						</xsl:choose>
					</metadata>
					<content>
						<xsl:apply-templates select="p|ulist|slist|blist"/>
					</content>
				</subsection>
			</xsl:if>
			<xsl:apply-templates select="scrap"/>
<!-- TODO: div3 etc!!! -->
			<xsl:apply-templates select="div3|div4|div5"/>
		</subsection>
	</xsl:template>
<!-- neglecting xlink, show, actuate properties-->
	<xsl:template match="loc">
		<ldf:link>
			<text>
				<xsl:value-of select="text()"/>
			</text>
			<external>
				<xsl:value-of select="@href"/>
			</external>
		</ldf:link>
	</xsl:template>
<!-- processing text -->
	<xsl:template match="p">
		<para>
			<xsl:apply-templates select="node()"/>
		</para>
	</xsl:template>
<!-- processing references -->
	<xsl:template match="bibref">
		<ldf:link>
			<text>
				<xsl:text>[</xsl:text>
				<xsl:value-of select="@ref"/>
				<xsl:text>]</xsl:text>
			</text>
			<reference>
				<xsl:value-of select="@ref"/>
			</reference>
		</ldf:link>
	</xsl:template>
	<xsl:template match="specref">
		<ldf:link>
			<text>
<!-- find the name of the section? -->
				<xsl:call-template name="getSectionName">
					<xsl:with-param name="id" select="@ref"/>
				</xsl:call-template>
			</text>
			<reference>
				<xsl:value-of select="@ref"/>
			</reference>
		</ldf:link>
	</xsl:template>
	<xsl:template match="xspecref">
		<ldf:link>
			<text>
				<xsl:value-of select="."/>
			</text>
			<external>
				<xsl:value-of select="@href"/>
			</external>
		</ldf:link>
	</xsl:template>
	<xsl:template match="xnt">
		<ldf:link>
			<text>
				<xsl:value-of select="."/>
			</text>
			<external>
				<xsl:value-of select="@href"/>
			</external>
		</ldf:link>
	</xsl:template>
	<xsl:template match="termref">
		<ldf:link>
			<text>
				<xsl:value-of select="."/>
			</text>
			<reference>
				<xsl:value-of select="@def"/>
			</reference>
		</ldf:link>
	</xsl:template>
<!-- processing lists -->
	<xsl:template match="slist">
		<list>
			<xsl:for-each select="sitem">
				<item>
					<xsl:apply-templates select="node()"/>
				</item>
			</xsl:for-each>
		</list>
	</xsl:template>
	<xsl:template match="ulist">
		<list>
			<xsl:for-each select="item">
				<item>
					<xsl:apply-templates select="p/node()"/>
				</item>
			</xsl:for-each>
		</list>
	</xsl:template>
	<xsl:template match="blist">
		<list>
			<xsl:for-each select="bibl">
				<item>
					<xsl:if test="@id">
						<anchor id="{@id}"/>
					</xsl:if>
					<xsl:apply-templates select="node()"/>
				</item>
			</xsl:for-each>
		</list>
	</xsl:template>
<!-- processing keywords -->
	<xsl:template match="term">
		<ldf:keyword>
			<xsl:apply-templates select="node()"/>
		</ldf:keyword>
	</xsl:template>
	<xsl:template match="code">
		<ldf:code>
			<xsl:apply-templates select="node()"/>
		</ldf:code>
	</xsl:template>
<!-- processing term definitions as achor + description -->
	<xsl:template match="termdef">
		<ldf:anchor id="{@id}"/>
		<xsl:apply-templates select="node()"/>
	</xsl:template>
<!-- general called templates -->
	<xsl:template name="getSectionName">
		<xsl:param name="id"/>
		<xsl:value-of select="//*[@id=$id]/head"/>
	</xsl:template>
<!--
  <scrap>
<head>Location Paths</head>
<prodgroup pcw5="1" pcw2="10" pcw4="18">
<prod id="NT-LocationPath">
<lhs>LocationPath</lhs>
<rhs><nt def="NT-RelativeLocationPath">RelativeLocationPath</nt></rhs>
<rhs>| <nt def="NT-AbsoluteLocationPath">AbsoluteLocationPath</nt></rhs>
</prod>
<prod id="NT-AbsoluteLocationPath">
<lhs>AbsoluteLocationPath</lhs>
<rhs>'/' <nt def="NT-RelativeLocationPath">RelativeLocationPath</nt>?</rhs>
<rhs>| <nt def="NT-AbbreviatedAbsoluteLocationPath">AbbreviatedAbsoluteLocationPath</nt></rhs>
</prod>
<prod id="NT-RelativeLocationPath">
<lhs>RelativeLocationPath</lhs>
<rhs><nt def="NT-Step">Step</nt></rhs>
<rhs>| <nt def="NT-RelativeLocationPath">RelativeLocationPath</nt> '/' <nt def="NT-Step">Step</nt></rhs>
<rhs>| <nt def="NT-AbbreviatedRelativeLocationPath">AbbreviatedRelativeLocationPath</nt></rhs>
</prod>
</prodgroup>
</scrap>
-->
	<xsl:template match="scrap">
		<subsection>
			<metadata>
				<role>syntax</role>
				<type>normative</type>
				<title>
					<xsl:value-of select="head"/>
				</title>
			</metadata>
			<content>
				<xsl:for-each select="prod|prodgroup/prod">
					<xsl:call-template name="copy-production">
						<xsl:with-param name="id" select="@id"/>
					</xsl:call-template>
				</xsl:for-each>
			</content>
		</subsection>
	</xsl:template>
	<xsl:template name="copy-production">
		<xsl:param name="id"/>
		<xsl:copy-of select="$bgf//bgf:production[label=$id]"/>
	</xsl:template>
	<xsl:template match="prod">
<!--
      not needed anymore:
      <ldf:anchor id="{@id}"/>
    -->
		<xsl:call-template name="copy-production">
			<xsl:with-param name="id" select="@id"/>
		</xsl:call-template>
	</xsl:template>
<!--
  <rhs>
    <nt def="NT-RelativeLocationPath">RelativeLocationPath</nt>
  </rhs>
  <rhs>
    | <nt def="NT-AbsoluteLocationPath">AbsoluteLocationPath</nt>
  </rhs>
-->
</xsl:stylesheet>
