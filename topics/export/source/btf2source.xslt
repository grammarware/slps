<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:btf="http://planet-sl.org/btf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
	<xsl:variable name="apos">'</xsl:variable>
	<xsl:variable name="bs">\</xsl:variable>
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
				<xsl:variable name="trmQuotesGone">
					<xsl:call-template name="string-replace-all">
						<xsl:with-param name="text" select="terminal"/>
						<xsl:with-param name="replace" select="concat($bs,$apos)"/>
						<xsl:with-param name="by" select="$apos"/>
					</xsl:call-template>
				</xsl:variable>
				<xsl:call-template name="string-replace-all">
					<xsl:with-param name="text" select="$trmQuotesGone"/>
					<xsl:with-param name="replace" select="concat($bs,$bs)"/>
					<xsl:with-param name="by" select="$bs"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:when test="choice|selectable">
				<xsl:apply-templates select="*/btf:tree"/>
			</xsl:when>
			<xsl:when test="nonterminal">
				<xsl:choose>
					<xsl:when test="substring(nonterminal/bgf:production/nonterminal,1,1) = translate(substring(nonterminal/bgf:production/nonterminal,1,1), 'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')">
						<xsl:for-each select="*/btf:tree">
							<xsl:call-template name="lextree"/>
						</xsl:for-each>
					</xsl:when>
					<xsl:otherwise>
						<xsl:apply-templates select="*/btf:tree"/>
					</xsl:otherwise>
				</xsl:choose>
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
<!-- no whitespace for lexical nonterminals -->
	<xsl:template name="lextree">
		<xsl:choose>
			<xsl:when test="epsilon">
				<xsl:text/>
			</xsl:when>
			<xsl:when test="alpha">
				<xsl:text>α</xsl:text>
			</xsl:when>
			<xsl:when test="terminal">
				<xsl:variable name="trmQuotesGone">
					<xsl:call-template name="string-replace-all">
						<xsl:with-param name="text" select="terminal"/>
						<xsl:with-param name="replace" select="concat($bs,$apos)"/>
						<xsl:with-param name="by" select="$apos"/>
					</xsl:call-template>
				</xsl:variable>
				<xsl:call-template name="string-replace-all">
					<xsl:with-param name="text" select="$trmQuotesGone"/>
					<xsl:with-param name="replace" select="concat($bs,$bs)"/>
					<xsl:with-param name="by" select="$bs"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:when test="nonterminal|choice|selectable|sequence|star|plus|optional">
				<xsl:for-each select="*/btf:tree">
					<xsl:call-template name="lextree"/>
				</xsl:for-each>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="local-name(*)"/>
				<xsl:text>?</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
<!-- taken from http://geekswithblogs.net/Erik/archive/2008/04/01/120915.aspx -->
	<xsl:template name="string-replace-all">
		<xsl:param name="text"/>
		<xsl:param name="replace"/>
		<xsl:param name="by"/>
		<xsl:choose>
			<xsl:when test="contains($text, $replace)">
				<xsl:value-of select="substring-before($text,$replace)"/>
				<xsl:value-of select="$by"/>
				<xsl:call-template name="string-replace-all">
					<xsl:with-param name="text" select="substring-after($text,$replace)"/>
					<xsl:with-param name="replace" select="$replace"/>
					<xsl:with-param name="by" select="$by"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="$text"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
