<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:ldf="http://planet-sl.org/ldf" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
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
			<ldf:part>
				<role>front-matter</role>
				<ldf:metainfo>
					<title>Status of this document</title>
				</ldf:metainfo>
				<content>
					<simple-section>
						<xsl:copy-of select="header/status"/>
					</simple-section>
				</content>
			</ldf:part>
		</ldf:document>
	</xsl:template>
</xsl:stylesheet>
