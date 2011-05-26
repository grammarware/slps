<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="text" encoding="UTF-8"/>
	<xsl:template match="/zoo">
		<xsl:text>all:
</xsl:text>
		<xsl:for-each select="language">
			<xsl:for-each select="version/grammar">
				<xsl:text>	_dev/add </xsl:text>
				<xsl:value-of select="/zoo/name"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="../../handle"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="handle"/>
				<xsl:text>
</xsl:text>
				<xsl:text>	_dev/format </xsl:text>
				<xsl:value-of select="/zoo/name"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="../../handle"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="handle"/>
				<xsl:text>
</xsl:text>
			</xsl:for-each>
		</xsl:for-each>
	</xsl:template>
</xsl:stylesheet>
