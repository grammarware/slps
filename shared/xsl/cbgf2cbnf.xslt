<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:cbgf="http://planet-sl.org/cbgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:import href="bgf2bnf.xslt"/>
	<xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
	<xsl:template match="/cbgf:relationship">
		<xsl:apply-templates select="./cbgf:*"/>
	</xsl:template>
	<!-- optional context -->
	<xsl:template name="context">
		<xsl:param name="in"/>
		<xsl:choose>
			<xsl:when test="$in/nonterminal">
				<xsl:text> in </xsl:text>
				<xsl:value-of select="$in/nonterminal"/>
			</xsl:when>
			<xsl:when test="$in/label">
				<xsl:text> in [</xsl:text>
				<xsl:value-of select="$in/label"/>
				<xsl:text>]</xsl:text>
			</xsl:when>
		</xsl:choose>
	</xsl:template>
	<!-- chain, define, ... -->
	<xsl:template match="cbgf:*">
		<xsl:value-of select="local-name()"/>
		<xsl:text>(
</xsl:text>
		<xsl:call-template name="cse">
			<xsl:with-param name="lst" select="*"/>
		</xsl:call-template>
		<xsl:text>);
</xsl:text>
	</xsl:template>
	<xsl:template match="cbgf:rename-rename">
		<xsl:choose>
			<xsl:when test="local-name(*[1]) = 'nonterminal'">
				<xsl:text>rename-renameN</xsl:text>
			</xsl:when>
			<xsl:when test="local-name(*[1]) = 'label'">
				<xsl:text>rename-renameL</xsl:text>
			</xsl:when>
			<xsl:when test="local-name(*[1]) = 'selector'">
				<xsl:text>rename-renameS</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="local-name()"/>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text>(</xsl:text>
		<xsl:value-of select="*/from"/>
		<xsl:text>, </xsl:text>
		<xsl:value-of select="*/to"/>
		<xsl:text>);
</xsl:text>
	</xsl:template>
	<xsl:template match="cbgf:vertical-horizontal|cbgf:horizontal-vertical">
		<xsl:value-of select="local-name()"/>
		<xsl:text>(</xsl:text>
		<xsl:value-of select="nonterminal"/>
		<xsl:text>);
</xsl:text>
	</xsl:template>
	<xsl:template match="cbgf:reroot-reroot">
		<xsl:value-of select="local-name()"/>
		<xsl:text>(
	[</xsl:text>
		<xsl:call-template name="csv">
			<xsl:with-param name="lst" select="from/root"/>
			<xsl:with-param name="sep" select="', '"/>
		</xsl:call-template>
		<xsl:text>],
	[</xsl:text>
		<xsl:call-template name="csv">
			<xsl:with-param name="lst" select="to/root"/>
			<xsl:with-param name="sep" select="', '"/>
		</xsl:call-template>
		<xsl:text>]
);
</xsl:text>
	</xsl:template>
	<!-- secondary functions -->
	<xsl:template name="csv">
		<xsl:param name="lst"/>
		<xsl:param name="sep"/>
		<xsl:value-of select="$lst[1]"/>
		<xsl:for-each select="$lst[position() &gt; 1]">
			<xsl:value-of select="$sep"/>
			<xsl:value-of select="."/>
		</xsl:for-each>
	</xsl:template>
	<xsl:template name="cse">
		<xsl:param name="lst"/>
				<xsl:text> </xsl:text>
		<xsl:apply-templates select="$lst[1]"/>
		<xsl:for-each select="$lst[position() &gt; 1]">
			<xsl:text>,
 </xsl:text>
			<xsl:apply-templates select="."/>
		</xsl:for-each>
	</xsl:template>
</xsl:stylesheet>
