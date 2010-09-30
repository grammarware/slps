<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
	<xsl:template match="/bgf:grammar">
		<xsl:text>%% GENERATED GRAMMAR
#pragma -newline
</xsl:text>
		<xsl:if test="//terminal[string-length(.) &gt; 1]">
			<xsl:text>compounds
</xsl:text>
			<xsl:for-each select="//terminal[string-length(.) &gt; 1]">
				<xsl:value-of select="."/>
				<xsl:text> </xsl:text>
			</xsl:for-each>
			<xsl:text>end compounds
</xsl:text>
		</xsl:if>
<!-- todo later -->
		<xsl:apply-templates select="./bgf:*"/>
		<xsl:if test="//root">
			<xsl:text>
function main match [</xsl:text>
			<xsl:value-of select="//root[1]"/>
			<xsl:text>] _ [</xsl:text>
			<xsl:value-of select="//root[1]"/>
			<xsl:text>] end function
		</xsl:text>
		</xsl:if>
	</xsl:template>
	<xsl:template match="bgf:production">
		<xsl:text>define </xsl:text>
		<xsl:value-of select="./nonterminal"/>
		<xsl:text>
		</xsl:text>
<!-- what to do with labels? TODO 
		<xsl:if test="./label">
			<xsl:text>[</xsl:text>
			<xsl:value-of select="./label"/>
			<xsl:text>] </xsl:text>
		</xsl:if>-->
		<xsl:choose>
			<xsl:when test="./bgf:expression/choice">
				<xsl:for-each select="./bgf:expression/choice/bgf:expression">
					<xsl:if test="position() != 1">
						<xsl:text>
     |   </xsl:text>
					</xsl:if>
					<xsl:call-template name="no-parenthesis">
						<xsl:with-param name="expr" select="."/>
					</xsl:call-template>
				</xsl:for-each>
				<xsl:text>
</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:call-template name="no-parenthesis">
					<xsl:with-param name="expr" select="./bgf:expression"/>
				</xsl:call-template>
				<xsl:text>
</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text>end define
</xsl:text>
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
		<xsl:text>[</xsl:text>
		<xsl:apply-templates select="./*"/>
		<xsl:text>+]</xsl:text>
	</xsl:template>
	<xsl:template match="star">
		<xsl:text>[</xsl:text>
		<xsl:apply-templates select="./*"/>
		<xsl:text>*]</xsl:text>
	</xsl:template>
	<xsl:template match="optional">
		<xsl:apply-templates select="./*"/>
		<xsl:text>?</xsl:text>
	</xsl:template>
	<xsl:template match="terminal">
		<xsl:value-of select="."/>
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
		<xsl:text>[empty]</xsl:text>
	</xsl:template>
	<xsl:template match="empty">
		<xsl:text>EMPTY</xsl:text>
	</xsl:template>
	<xsl:template match="any">
		<xsl:text>ANY</xsl:text>
	</xsl:template>
	<xsl:template match="nonterminal">
		<xsl:if test="local-name(../..) != 'plus' and local-name(../..) != 'star'">
			<xsl:text>[</xsl:text>
		</xsl:if>
		<xsl:value-of select="."/>
		<xsl:if test="local-name(../..) != 'plus' and local-name(../..) != 'star'">
			<xsl:text>]</xsl:text>
		</xsl:if>
	</xsl:template>
	<xsl:template match="selectable">
		<xsl:value-of select="selector"/>
		<xsl:text>::</xsl:text>
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
		<!--<xsl:text>(</xsl:text> -->
		<xsl:apply-templates select="./bgf:expression[1]/*"/>
		<xsl:for-each select="./bgf:expression[position()&gt;1]">
			<xsl:text> | </xsl:text>
			<xsl:apply-templates select="./*"/>
		</xsl:for-each>
		<!--<xsl:text>)</xsl:text>-->
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
