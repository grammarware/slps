<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:edd="http://planet-sl.org/edd" version="1.0">
	<xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
	<xsl:template match="/edd:config">
		<xsl:for-each select="*">
			<xsl:choose>
				<xsl:when test="local-name()='ignore'">
					<xsl:value-of select="local-name()"/>
					<xsl:text>=</xsl:text>
					<xsl:value-of select="lines-containing"/>
					<xsl:text>
</xsl:text>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="local-name()"/>
					<xsl:text>=</xsl:text>
					<xsl:value-of select="."/>
					<xsl:text>
</xsl:text>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:for-each>
	</xsl:template>
</xsl:stylesheet>
