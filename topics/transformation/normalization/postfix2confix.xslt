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
				<xsl:when test="choice and count(*) = 1">
					<choice>
						<xsl:apply-templates select="choice/*"/>
					</choice>
				</xsl:when>
				<xsl:when test="sequence and count(*) = 1">
					<sequence>
						<xsl:apply-templates select="sequence/*"/>
					</sequence>
				</xsl:when>
				<xsl:when test="postfix-optional or postfix-star">
					<xsl:for-each select="*">
						<xsl:choose>
							<xsl:when test="local-name(following-sibling::*[1]) = 'postfix-optional'">
								<optional>
									<xsl:apply-templates select="."/>
								</optional>
							</xsl:when>
							<xsl:when test="local-name(following-sibling::*[1]) = 'postfix-star'">
								<star>
									<xsl:apply-templates select="."/>
								</star>
							</xsl:when>
							<xsl:when test="local-name(.) = 'postfix-optional'"/>
							<xsl:when test="local-name(.) = 'postfix-star'"/>
							<xsl:otherwise>
								<xsl:apply-templates select="."/>
							</xsl:otherwise>
						</xsl:choose>
					</xsl:for-each>
				</xsl:when>
				<xsl:otherwise>
					<xsl:apply-templates select="*"/>
				</xsl:otherwise>
			</xsl:choose>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="nonterminal|terminal|unknown">
		<xsl:copy-of select="."/>
	</xsl:template>
	<xsl:template match="sequence">
		<sequence>
			<xsl:apply-templates select="*"/>
		</sequence>
	</xsl:template>
</xsl:stylesheet>
