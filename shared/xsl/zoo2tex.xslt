<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
	<xsl:template match="/zoo">
		<xsl:text>{\footnotesize
			\begin{longtable}{|l|l|l|l|l|}\hline
			\textbf{Language}	&amp;	\textbf{Source}	&amp;	\textbf{To L0}	&amp;	\textbf{To L1}	&amp;	\textbf{To L2}\\
			\hline
		</xsl:text>
		<xsl:for-each select="language">
			<xsl:for-each select="version">
				<xsl:variable name="name" select="name"/>
				<xsl:for-each select="expand-grammar">
				<xsl:call-template name="tablerow">
					<xsl:with-param name="name" select="$name"/>
					<xsl:with-param name="meta" select="document(concat('/Users/zaytsev/projects/slps/topics/grammars/',.,'/zoo.xml'))/grammar"/>
				</xsl:call-template>
			</xsl:for-each>
			</xsl:for-each>
			<xsl:text>\hline
</xsl:text>
		</xsl:for-each>
		<xsl:text>\end{longtable}}</xsl:text>
	</xsl:template>
	<xsl:template name="tablerow">
		<xsl:param name="name"/>
		<xsl:param name="meta"/>
		<xsl:value-of select="translate($name,'#','s')"/>
		<xsl:text>	&amp;	</xsl:text>
		<xsl:choose>
			<xsl:when test="$meta/meta/src"><xsl:value-of select="$meta/meta/src"/></xsl:when>
			<xsl:otherwise>???</xsl:otherwise>
		</xsl:choose>
		<xsl:text>	&amp;	</xsl:text>
		<xsl:choose>
			<xsl:when test="$meta/meta/tol0=''">done</xsl:when>
			<xsl:when test="$meta/meta/tol0"><xsl:value-of select="$meta/meta/tol0"/></xsl:when>
			<xsl:otherwise>---</xsl:otherwise>
		</xsl:choose>
		<xsl:text>	&amp;	</xsl:text>
		<xsl:choose>
			<xsl:when test="$meta/meta/tol1"><xsl:value-of select="$meta/meta/tol1"/></xsl:when>
			<xsl:otherwise>---</xsl:otherwise>
		</xsl:choose>
		<xsl:text>	&amp;	</xsl:text>
		<xsl:choose>
			<xsl:when test="$meta/meta/tol2"><xsl:value-of select="$meta/meta/tol2"/></xsl:when>
			<xsl:otherwise>---</xsl:otherwise>
		</xsl:choose>
		<xsl:text>	\\
</xsl:text>
	</xsl:template>
</xsl:stylesheet>
