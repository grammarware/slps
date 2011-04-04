<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:bgf="http://planet-sl.org/bgf"
	xmlns:btf="http://planet-sl.org/btf"
	xmlns:xhtml="http://www.w3.org/1999/xhtml"
	version="1.0">
	<xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
	<xsl:template match="/btf:root">
		<xsl:apply-templates select="./btf:tree"/>
		<xsl:text>
</xsl:text>
	</xsl:template>
	<xsl:template match="btf:tree">
		<xsl:choose>
			<xsl:when test="epsilon">
				<xsl:text/>
			</xsl:when>
			<xsl:when test="alpha">
				<xsl:text>α</xsl:text>
			</xsl:when>
			<xsl:when test="terminal">
				<xsl:value-of select="terminal"/>
			</xsl:when>
			<xsl:when test="nonterminal|choice|selectable">
				<xsl:apply-templates select="*/btf:tree"/>
			</xsl:when>
			<xsl:when test="sequence|star|plus|optional">
				<xsl:if test="*/btf:tree">
					<xsl:for-each select="*/btf:tree[1]">
						<xsl:apply-templates select="."/>
					</xsl:for-each>
					<xsl:for-each select="*/btf:tree[position()&gt;1]">
						<xsl:text> </xsl:text>
						<xsl:apply-templates select="."/>
					</xsl:for-each>
				</xsl:if>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="local-name(*)"/>
				<xsl:text>?</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
