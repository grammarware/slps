<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:cbgf="http://planet-sl.org/cbgf" xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/bgf:grammar">
		<xbgf:sequence xmlns:xbgf="http://planet-sl.org/xbgf">
			<xsl:for-each select="root">
				<xsl:call-template name="expose-nonterminal">
					<xsl:with-param name="nt" select="."/>
				</xsl:call-template>
			</xsl:for-each>
		</xbgf:sequence>
	</xsl:template>
	<xsl:template name="expose-nonterminal">
		<xsl:param name="nt"/>
		<xsl:if test="count(/bgf:grammar/bgf:production[nonterminal=$nt]/bgf:expression//nonterminal)=1">
			<xbgf:inline xmlns:xbgf="http://planet-sl.org/xbgf">
				<xsl:value-of select="/bgf:grammar/bgf:production[nonterminal=$nt]/bgf:expression//nonterminal"/>
			</xbgf:inline>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet>
