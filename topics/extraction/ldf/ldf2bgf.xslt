<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:ldf="http://planet-sl.org/ldf" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/ldf:document">
		<bgf:grammar>
			<xsl:apply-templates select="//bgf:production"/>
		</bgf:grammar>
	</xsl:template>
	<xsl:template match="bgf:production">
		<xsl:if test="local-name(../..) != 'example'">
			<xsl:copy-of select="."/>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet>
