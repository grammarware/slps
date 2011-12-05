<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:edd="http://planet-sl.org/edd" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
	<xsl:param name="edd"/>
	<xsl:variable name="cfg" select="document($edd)/edd:config"/>
	<xsl:template match="/bgf:grammar">
		<xsl:apply-templates select="./bgf:*"/>
	</xsl:template>
	<xsl:template match="bgf:production">
		<xsl:if test="./label">
			<xsl:value-of select="$cfg/start-label-symbol"/>
			<xsl:value-of select="./label"/>
			<xsl:value-of select="$cfg/end-label-symbol"/>
		</xsl:if>
		<xsl:value-of select="./nonterminal"/>
		<xsl:text> </xsl:text>
		<xsl:value-of select="$cfg/defining-symbol"/>
		<xsl:text> </xsl:text>
		<xsl:choose>
			<xsl:when test="./bgf:expression/choice">
				<xsl:call-template name="no-parenthesis">
					<xsl:with-param name="expr" select="bgf:expression/choice/bgf:expression[1]"/>
				</xsl:call-template>
				<xsl:for-each select="./bgf:expression/choice/bgf:expression[position()&gt;1]">
					<xsl:if test="not(contains($cfg/definition-separator-symbol,' '))">
						<xsl:text> </xsl:text>
					</xsl:if>
					<xsl:value-of select="$cfg/definition-separator-symbol"/>
					<xsl:if test="not(contains($cfg/definition-separator-symbol,' '))">
						<xsl:text> </xsl:text>
					</xsl:if>
					<xsl:call-template name="no-parenthesis">
						<xsl:with-param name="expr" select="."/>
					</xsl:call-template>
				</xsl:for-each>
			</xsl:when>
			<xsl:otherwise>
				<xsl:call-template name="no-parenthesis">
					<xsl:with-param name="expr" select="./bgf:expression"/>
				</xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:if test="$cfg/terminator-symbol and not($cfg/terminator-symbol='\n')">
			<xsl:text> </xsl:text>
		</xsl:if>
		<xsl:value-of select="$cfg/terminator-symbol"/>
		<xsl:if test="not(contains($cfg/terminator-symbol,'\n'))">
			<xsl:text>
</xsl:text>
		</xsl:if>
	</xsl:template>
	<xsl:template match="bgf:expression">
		<xsl:apply-templates select="./*"/>
	</xsl:template>
	<xsl:template match="marked">
		<xsl:text><![CDATA[<]]></xsl:text>
		<xsl:apply-templates select="./*"/>
		<xsl:text><![CDATA[>]]></xsl:text>
	</xsl:template>
	<xsl:template match="plus">
		<xsl:choose>
			<xsl:when test="$cfg/postfix-repetition-plus-symbol">
				<xsl:apply-templates select="./*"/>
				<xsl:value-of select="$cfg/postfix-repetition-plus-symbol"/>
			</xsl:when>
			<xsl:when test="$cfg/start-repetition-plus-symbol">
				<xsl:value-of select="$cfg/start-repetition-plus-symbol"/>
				<xsl:apply-templates select="./*"/>
				<xsl:value-of select="$cfg/end-repetition-plus-symbol"/>
			</xsl:when>
			<xsl:otherwise>UNKNOWN OPERATION: PLUS</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="seplistplus">
		<xsl:choose>
			<xsl:when test="$cfg/start-seplist-plus-symbol">
				<xsl:value-of select="$cfg/start-seplist-plus-symbol"/>
				<xsl:apply-templates select="./*"/>
				<xsl:value-of select="$cfg/start-seplist-plus-symbol"/>
			</xsl:when>
			<xsl:otherwise>UNKNOWN OPERATION: SEP LIST PLUS</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="sepliststar">
		<xsl:choose>
			<xsl:when test="$cfg/start-seplist-star-symbol">
				<xsl:value-of select="$cfg/start-seplist-star-symbol"/>
				<xsl:apply-templates select="./*"/>
				<xsl:value-of select="$cfg/start-seplist-star-symbol"/>
			</xsl:when>
			<xsl:otherwise>UNKNOWN OPERATION: SEP LIST STAR</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="star">
		<xsl:choose>
			<xsl:when test="$cfg/postfix-repetition-star-symbol">
				<xsl:apply-templates select="./*"/>
				<xsl:value-of select="$cfg/postfix-repetition-star-symbol"/>
			</xsl:when>
			<xsl:when test="$cfg/start-repetition-star-symbol">
				<xsl:value-of select="$cfg/start-repetition-star-symbol"/>
				<xsl:apply-templates select="./*"/>
				<xsl:value-of select="$cfg/end-repetition-star-symbol"/>
			</xsl:when>
			<xsl:otherwise>UNKNOWN OPERATION: STAR</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="optional">
		<xsl:apply-templates select="./*"/>
		<xsl:text>?</xsl:text>
	</xsl:template>
	<xsl:template match="terminal">
		<xsl:value-of select="$cfg/start-terminal-symbol"/>
		<xsl:value-of select="."/>
		<xsl:value-of select="$cfg/end-terminal-symbol"/>
	</xsl:template>
	<xsl:template match="value">
		<xsl:choose>
			<xsl:when test=". = 'string'">
				<xsl:text>STR</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>INT</xsl:text>
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
		<xsl:value-of select="$cfg/start-nonterminal-symbol"/>
		<xsl:value-of select="."/>
		<xsl:value-of select="$cfg/end-nonterminal-symbol"/>
	</xsl:template>
	<xsl:template match="selectable">
		<!-- TODO
		<xsl:value-of select="selector"/>
		<xsl:text>::</xsl:text>
		-->
		<xsl:choose>
			<xsl:when test="local-name(bgf:expression/*) = 'star'                    or local-name(bgf:expression/*) = 'optional'                    or local-name(bgf:expression/*) = 'plus'">
				<xsl:text>(</xsl:text>
				<xsl:apply-templates select="bgf:expression"/>
				<xsl:text>)</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates select="bgf:expression"/>
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
