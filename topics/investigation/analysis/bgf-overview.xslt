<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
	<xsl:template match="/bgf:grammar">
		<xsl:text>Total productions:    </xsl:text>
		<xsl:value-of select="count(/*/*[local-name()='production'])"/>
		<xsl:text>
Total nonterminals:   </xsl:text>
		<xsl:value-of select="count(/*[local-name()='grammar']/*/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())])"/>
		<xsl:text>
Top nonterminals:     </xsl:text>
		<xsl:value-of select="count(/*/*/nonterminal[not(text()=/*/*/*//nonterminal/text()) and not(text()=../preceding-sibling::*/nonterminal/text())])"/>
		<xsl:text> (</xsl:text>
		<xsl:for-each select="/*/*/nonterminal[not(text()=/*/*/*//nonterminal/text()) and not(text()=../preceding-sibling::*/nonterminal/text())]">
			<xsl:value-of select="."/>
			<xsl:if test="position()!=last()">
				<xsl:text>, </xsl:text>
			</xsl:if>
		</xsl:for-each>
		<xsl:text>)</xsl:text>
		<xsl:text>
Bottom nonterminals:  </xsl:text>
		<xsl:value-of select="count(/*/*[not(*//nonterminal)]/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())])"/>
		<xsl:text> (</xsl:text>
		<xsl:for-each select="/*/*[not(*//nonterminal)]/nonterminal[not(text()=../preceding-sibling::*/nonterminal/text())]">
			<xsl:value-of select="."/>
			<xsl:if test="position()!=last()">
				<xsl:text>, </xsl:text>
			</xsl:if>
		</xsl:for-each>
		<xsl:text>)</xsl:text>
		<xsl:text>
</xsl:text>
	</xsl:template>
</xsl:stylesheet>
