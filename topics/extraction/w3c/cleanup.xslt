<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/bgf:grammar">
		<bgf:grammar xmlns:bgf="http://planet-sl.org/bgf">
			<xsl:for-each select="node()">
				<xsl:choose>
					<xsl:when test="local-name(.)='production'">
						<xsl:apply-templates select="."/>
					</xsl:when>
					<xsl:otherwise>
						<xsl:copy-of select="."/>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:for-each>
		</bgf:grammar>
	</xsl:template>
	<xsl:template match="bgf:production">
		<bgf:production>
			<xsl:copy-of select="label"/>
			<xsl:copy-of select="nonterminal"/>
			<xsl:apply-templates select="bgf:expression"/>
		</bgf:production>
	</xsl:template>
	<xsl:template match="bgf:expression">
		<bgf:expression>
			<xsl:choose>
				<xsl:when test="choice">
					<choice>
						<xsl:apply-templates select="choice/*"/>
					</choice>
				</xsl:when>
				<xsl:when test="sequence">
					<sequence>
						<xsl:apply-templates select="sequence/*"/>
					</sequence>
				</xsl:when>
				<xsl:when test="count(*) &gt; 1 and not(unknown)">
					<sequence>
						<xsl:for-each select="*">
							<bgf:expression>
								<xsl:apply-templates select="."/>
							</bgf:expression>
						</xsl:for-each>
					</sequence>
				</xsl:when>
				<xsl:when test="count(*) &gt; 1 and unknown = '|'">
					<choice>
						<xsl:for-each select="*[local-name(.)!='unknown']">
							<bgf:expression>
								<xsl:apply-templates select="."/>
							</bgf:expression>
						</xsl:for-each>
					</choice>
				</xsl:when>
				<xsl:when test="count(*) &gt; 1 and unknown != '|'">
<!-- the extractor gives up -->
					<epsilon/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:apply-templates select="*"/>
				</xsl:otherwise>
			</xsl:choose>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="nonterminal|terminal">
		<xsl:copy-of select="."/>
	</xsl:template>
	<xsl:template match="optional">
		<optional>
			<xsl:choose>
				<xsl:when test="bgf:expression|sequence">
					<xsl:apply-templates select="*"/>
				</xsl:when>
				<xsl:otherwise>
					<bgf:expression>
						<xsl:apply-templates select="*"/>
					</bgf:expression>
				</xsl:otherwise>
			</xsl:choose>
		</optional>
	</xsl:template>
	<xsl:template match="plus">
		<plus>
			<xsl:choose>
				<xsl:when test="bgf:expression|sequence">
					<xsl:apply-templates select="*"/>
				</xsl:when>
				<xsl:otherwise>
					<bgf:expression>
						<xsl:apply-templates select="*"/>
					</bgf:expression>
				</xsl:otherwise>
			</xsl:choose>
		</plus>
	</xsl:template>
	<xsl:template match="star">
		<star>
			<xsl:choose>
				<xsl:when test="bgf:expression|sequence">
					<xsl:apply-templates select="*"/>
				</xsl:when>
				<xsl:otherwise>
					<bgf:expression>
						<xsl:apply-templates select="*"/>
					</bgf:expression>
				</xsl:otherwise>
			</xsl:choose>
		</star>
	</xsl:template>
	<xsl:template match="unknown">
<!--			<xsl:value-of select="."/> -->
		<epsilon/>
	</xsl:template>
</xsl:stylesheet>
