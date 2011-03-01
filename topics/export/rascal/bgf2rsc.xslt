<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
	<xsl:template match="/bgf:grammar">
		<xsl:text>module ExtractedGrammar

</xsl:text>
		<xsl:apply-templates select="./bgf:*"/>
	</xsl:template>
	<xsl:template match="bgf:production">
		<xsl:if test="./label">
			<xsl:text>[</xsl:text>
			<xsl:value-of select="./label"/>
			<xsl:text>] </xsl:text>
		</xsl:if>
		<xsl:text>syntax </xsl:text>
		<xsl:value-of select="./nonterminal"/>
		<xsl:text>
        = </xsl:text>
		<xsl:choose>
			<xsl:when test="./bgf:expression/choice">
				<xsl:for-each select="./bgf:expression/choice/bgf:expression">
					<xsl:if test="position()!= 1">
						<xsl:text>
        | </xsl:text>
					</xsl:if>
					<xsl:call-template name="no-parenthesis">
						<xsl:with-param name="expr" select="."/>
					</xsl:call-template>
				</xsl:for-each>
				<xsl:text>
</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>
        </xsl:text>
				<xsl:call-template name="no-parenthesis">
					<xsl:with-param name="expr" select="./bgf:expression"/>
				</xsl:call-template>
				<xsl:text>
</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text> ;
</xsl:text>
	</xsl:template>
	<xsl:template match="bgf:expression">
		<xsl:apply-templates select="./*"/>
	</xsl:template>
	<xsl:template match="marked">
<!-- do not exist in Rascal -->
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="./*"/>
		<xsl:text>)</xsl:text>
	</xsl:template>
	<xsl:template match="plus">
		<xsl:apply-templates select="./*"/>
		<xsl:text>+</xsl:text>
	</xsl:template>
	<xsl:template match="star">
		<xsl:apply-templates select="./*"/>
		<xsl:text>*</xsl:text>
	</xsl:template>
	<xsl:template match="optional">
<!--  (N ("," N)*)? is treated as {N ","}* -->
		<xsl:choose>
			<xsl:when test="local-name(./bgf:expression/*[1]) = 'sequence'
                        and ./bgf:expression/sequence/bgf:expression[1]/*[1] = ./bgf:expression/sequence/bgf:expression[2]/star/bgf:expression/sequence/bgf:expression[2]/*[1]">
				<xsl:text>{</xsl:text>
				<xsl:apply-templates select="./bgf:expression/sequence/bgf:expression[1]"/>
				<xsl:text> </xsl:text>
				<xsl:apply-templates select="./bgf:expression/sequence/bgf:expression[2]/star/bgf:expression/sequence/bgf:expression[1]"/>
				<xsl:text>}*</xsl:text>
			</xsl:when>
			<!-- TODO: N ("," N)*) is NOT treated as {N ","}+ -->
			<xsl:otherwise>
				<xsl:apply-templates select="./*"/>
				<xsl:text>?</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="terminal">
		<xsl:text>"</xsl:text>
		<xsl:value-of select="."/>
		<xsl:text>"</xsl:text>
	</xsl:template>
	<xsl:template match="value">
		<xsl:choose>
			<xsl:when test=". = 'string'">
				<xsl:text>String</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>Integer</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="epsilon">
		<xsl:text>EPSILON</xsl:text>
	</xsl:template>
	<xsl:template match="empty">
		<xsl:text>EMPTY</xsl:text>
	</xsl:template>
	<xsl:template match="any">
		<xsl:text>ANY</xsl:text>
	</xsl:template>
	<xsl:template match="nonterminal">
		<xsl:value-of select="."/>
	</xsl:template>
	<xsl:template match="selectable">
		<xsl:choose>
			<xsl:when test="local-name(bgf:expression/*) = 'star'
				         or local-name(bgf:expression/*) = 'optional'
				         or local-name(bgf:expression/*) = 'plus'">
				<xsl:text>&lt;</xsl:text>
				<xsl:apply-templates select="bgf:expression"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="selector"/>
				<xsl:text>&gt;</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates select="bgf:expression"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="selector"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="sequence">
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="./bgf:expression[1]/*"/>
		<xsl:for-each select="./bgf:expression[position()&gt;1]">
			<xsl:text> </xsl:text>
			<xsl:apply-templates select="./*"/>
		</xsl:for-each>
		<xsl:text>)</xsl:text>
	</xsl:template>
<!-- inner choices - BNF bar -->
	<xsl:template match="choice">
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="./bgf:expression[1]/*"/>
		<xsl:for-each select="./bgf:expression[position()&gt;1]">
			<xsl:text> | </xsl:text>
			<xsl:apply-templates select="./*"/>
		</xsl:for-each>
		<xsl:text>)</xsl:text>
	</xsl:template>
	<xsl:template name="no-parenthesis">
		<xsl:param name="expr"/>
		<xsl:choose>
			<xsl:when test="$expr/sequence">
				<xsl:apply-templates select="$expr/sequence/bgf:expression[1]/*"/>
				<xsl:for-each select="$expr/sequence/bgf:expression[position()&gt;1]">
					<xsl:text> </xsl:text>
					<xsl:apply-templates select="./*"/>
				</xsl:for-each>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates select="$expr"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
