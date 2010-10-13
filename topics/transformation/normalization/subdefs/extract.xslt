<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xbgf="http://planet-sl.org/xbgf" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/bgf:grammar">
		<xbgf:sequence xmlns:bgf="http://planet-sl.org/bgf" xmlns:xbgf="http://planet-sl.org/xbgf">
			<xsl:for-each select="bgf:production">
				<xsl:if test="bgf:expression/sequence//choice">
					<xsl:call-template name="extract">
						<xsl:with-param name="name" select="nonterminal"/>
						<xsl:with-param name="choices" select="bgf:expression/sequence//choice"/>
					</xsl:call-template>
				</xsl:if>
			</xsl:for-each>
		</xbgf:sequence>
	</xsl:template>
	<xsl:template name="extract">
		<xsl:param name="name"/>
		<xsl:param name="choices"/>
		<xsl:for-each select="$choices">
			<xbgf:extract xmlns:bgf="http://planet-sl.org/bgf" xmlns:xbgf="http://planet-sl.org/xbgf">
				<bgf:production>
					<nonterminal>
						<xsl:value-of select="$name"/>
						<xsl:text>_grp_</xsl:text>
						<xsl:value-of select="position()"/>
					</nonterminal>
					<bgf:expression>
						<xsl:copy-of select="."/>
					</bgf:expression>
				</bgf:production>
				<in>
					<nonterminal><xsl:value-of select="$name"/></nonterminal>
				</in>
			</xbgf:extract>
		</xsl:for-each>
	</xsl:template>
</xsl:stylesheet>
