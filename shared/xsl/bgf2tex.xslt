<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
	<xsl:template match="/bgf:grammar">
		<xsl:text>\begin{boxedminipage}{\textwidth}
\footnotesize
\begin{align*}
</xsl:text>
		<xsl:apply-templates select="./bgf:*"/>
		<xsl:text>\end{align*}
\end{boxedminipage}
</xsl:text>
	</xsl:template>
	<xsl:template match="bgf:production">
		<xsl:text>\p(</xsl:text>
		<xsl:choose>
			<xsl:when test="label">
				<xsl:text>\textit{</xsl:text>
				<xsl:value-of select="./label"/>
				<xsl:text>}</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>\varepsilon</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text>, \textit{</xsl:text>
		<xsl:value-of select="./nonterminal"/>
		<xsl:text>}, &amp; </xsl:text>
		<xsl:apply-templates select="./bgf:expression"/>
		<xsl:text>)\\
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
		<xsl:text>{+}(</xsl:text>
		<xsl:apply-templates select="./*"/>
		<xsl:text>)</xsl:text>
	</xsl:template>
	<xsl:template match="star">
		<xsl:text>{*}(</xsl:text>
		<xsl:apply-templates select="./*"/>
		<xsl:text>)</xsl:text>
	</xsl:template>
	<xsl:template match="sepliststar">
		<xsl:text>{</xsl:text>
		<xsl:apply-templates select="*[1]"/>
		<xsl:text> </xsl:text>
		<xsl:apply-templates select="*[2]"/>
		<xsl:text>}*</xsl:text>
	</xsl:template>
	<xsl:template match="seplistplus">
		<xsl:text>{</xsl:text>
		<xsl:apply-templates select="*[1]"/>
		<xsl:text> </xsl:text>
		<xsl:apply-templates select="*[2]"/>
		<xsl:text>}+</xsl:text>
	</xsl:template>
	<xsl:template match="optional">
		<xsl:text>{?}(</xsl:text>
		<xsl:apply-templates select="./*"/>
		<xsl:text>)</xsl:text>
	</xsl:template>
	<!-- Boolean grammars support -->
	<xsl:template match="not">
		<xsl:text>\neg(</xsl:text>
		<xsl:choose>
			<xsl:when test="bgf:expression/optional or bgf:expression/plus or bgf:expression/star">
				<xsl:text>(</xsl:text>
				<xsl:apply-templates select="./*"/>
				<xsl:text>)</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates select="./*"/>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text>)</xsl:text>
	</xsl:template>
	<xsl:template match="terminal">
		<xsl:text>\te(\textit{</xsl:text>
		<xsl:value-of select="."/>
		<xsl:text>})</xsl:text>
	</xsl:template>
	<xsl:template match="value">
		<!-- for these purposes, values (built-in types) are displayed as nonterminals -->
		<xsl:text>\textit{</xsl:text>
		<xsl:value-of select="."/>
		<xsl:text>}</xsl:text>
	</xsl:template>
	<xsl:template match="epsilon">
		<xsl:text>\varepsilon</xsl:text>
	</xsl:template>
	<xsl:template match="empty">
		<xsl:text>\varphi</xsl:text>
	</xsl:template>
	<xsl:template match="any">
		<xsl:text>\alpha</xsl:text>
	</xsl:template>
	<xsl:template match="nonterminal">
		<xsl:text>\n(\textit{</xsl:text>
		<xsl:value-of select="."/>
		<xsl:text>})</xsl:text>
	</xsl:template>
	<xsl:template match="selectable">
		<xsl:text>\s(\textit{</xsl:text>
		<xsl:value-of select="selector"/>
		<xsl:text>}, </xsl:text>
		<xsl:apply-templates select="bgf:expression"/>
		<xsl:text>)</xsl:text>
	</xsl:template>
	<xsl:template match="sequence">
		<xsl:apply-templates select="./bgf:expression[1]/*"/>
		<xsl:for-each select="./bgf:expression[position()&gt;1]">
			<xsl:text> \cdot </xsl:text>
			<xsl:apply-templates select="./*"/>
		</xsl:for-each>
	</xsl:template>
	<!-- inner choices - BNF bar -->
	<xsl:template match="choice">
		<xsl:text>\dis([</xsl:text>
		<xsl:apply-templates select="./bgf:expression[1]/*"/>
		<xsl:for-each select="./bgf:expression[position()&gt;1]">
			<xsl:text> ; </xsl:text>
			<xsl:apply-templates select="./*"/>
		</xsl:for-each>
		<xsl:text>])</xsl:text>
	</xsl:template>
	<!-- conjunctive grammars support -->
	<xsl:template match="allof">
		<xsl:text>\con([</xsl:text>
		<xsl:apply-templates select="./bgf:expression[1]/*"/>
		<xsl:for-each select="./bgf:expression[position()&gt;1]">
			<xsl:text> ; </xsl:text>
			<xsl:apply-templates select="./*"/>
		</xsl:for-each>
		<xsl:text>])</xsl:text>
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
