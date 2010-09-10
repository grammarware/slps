<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:ldf="http://planet-sl.org/ldf" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:param name="grammar"/>
	<xsl:variable name="bgf" select="document($grammar)/bgf:grammar"/>
	<xsl:template match="/spec">
		<ldf:document xmlns:ldf="http://planet-sl.org/ldf">
			<ldf:title-page>
				<xsl:if test="header/w3c-doctype">
					<ldf:body>w3c</ldf:body>
					<number>
						<xsl:value-of select="header/w3c-designation"/>
					</number>
				</xsl:if>
				<ldf:metainfo>
					<xsl:copy-of select="header/title"/>
					<xsl:for-each select="header/authlist/author">
						<author>
							<xsl:value-of select="name"/>
						</author>
					</xsl:for-each>
				</ldf:metainfo>
				<xsl:choose>
					<xsl:when test="header/w3c-doctype = 'W3C Recommendation'">
						<ldf:status>approved</ldf:status>
					</xsl:when>
					<xsl:otherwise>
						<ldf:status>unknown</ldf:status>
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
					<ldf:previous>
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
					</ldf:previous>
				</xsl:for-each>
				<date>
					<xsl:value-of select="header/pubdate/day"/>
					<xsl:text> </xsl:text>
					<xsl:value-of select="header/pubdate/month"/>
					<xsl:text> </xsl:text>
					<xsl:value-of select="header/pubdate/year"/>
				</date>
			</ldf:title-page>
			<content>
				<ldf:part>
					<role>front-matter</role>
					<ldf:metainfo/>
					<content>
						<ldf:simple-section>
							<role>scope</role>
							<ldf:metainfo>
								<title>Status of this document</title>
							</ldf:metainfo>
							<content>
								<xsl:for-each select="header/status/p">
									<text>
										<xsl:apply-templates select="node()"/>
									</text>
								</xsl:for-each>
							</content>
						</ldf:simple-section>
						<ldf:simple-section>
							<role>abstract</role>
							<ldf:metainfo/>
							<content>
								<xsl:for-each select="header/abstract/p">
									<text>
										<xsl:apply-templates select="node()"/>
									</text>
								</xsl:for-each>
							</content>
						</ldf:simple-section>
<!-- skipped: langusage, revisiondesc -->
						<ldf:placeholder>list-of-contents</ldf:placeholder>
					</content>
				</ldf:part>
				<ldf:part>
					<role>core-part</role>
					<ldf:metainfo/>
					<content>
						<xsl:for-each select="body/div1">
							<xsl:choose>
								<xsl:when test="head = 'Introduction'">
									<ldf:simple-section>
										<role>foreword</role>
										<ldf:metainfo>
											<xsl:if test="@id">
												<id>
													<xsl:value-of select="@id"/>
												</id>
											</xsl:if>
											<title>
												<xsl:value-of select="head"/>
											</title>
										</ldf:metainfo>
										<content>
											<xsl:apply-templates select="*[local-name() != 'head']"/>
										</content>
									</ldf:simple-section>
								</xsl:when>
								<xsl:otherwise>
									<ldf:structured-section>
										<ldf:metainfo>
											<xsl:if test="@id">
												<id>
													<xsl:value-of select="@id"/>
												</id>
											</xsl:if>
											<title>
												<xsl:value-of select="head"/>
											</title>
										</ldf:metainfo>
										<content>
											<ldf:normative-role>description</ldf:normative-role>
											<ldf:simple-section>
												<role>abstract</role>
<!-- TODO -->
												<ldf:metainfo/>
												<content>
													<xsl:apply-templates select="*[local-name() != 'head']"/>
												</content>
											</ldf:simple-section>
										</content>
									</ldf:structured-section>
								</xsl:otherwise>
							</xsl:choose>
						</xsl:for-each>
					</content>
				</ldf:part>
			</content>
		</ldf:document>
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
		<text>
			<xsl:apply-templates select="node()"/>
		</text>
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
		<ldf:normative-role>syntax</ldf:normative-role>
		<ldf:simple-section>
			<role>abstract</role>
<!-- TODO -->
			<ldf:metainfo>
				<title>
					<xsl:value-of select="head"/>
				</title>
			</ldf:metainfo>
			<content>
				<xsl:apply-templates select="prodgroup/prod"/>
			</content>
		</ldf:simple-section>
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
