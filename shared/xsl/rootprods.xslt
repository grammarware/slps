<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" version="1.0">
	<xsl:import href="bgf2bnf.xslt"/>
	<xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
	<xsl:template match="/bgf:grammar">
		<xsl:for-each select="root">
			<xsl:call-template name="showprods">
				<xsl:with-param name="nt" select="."/>
			</xsl:call-template>
		</xsl:for-each>
	</xsl:template>
	<xsl:template name="showprods">
		<xsl:param name="nt"/>
		<xsl:apply-templates select="/bgf:grammar/bgf:production[nonterminal=$nt]"/>
	</xsl:template>
</xsl:stylesheet>
