<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:cbgf="http://planet-sl.org/cbgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:param name="rename"/>
	<xsl:variable name="rnm" select="document($rename)/cbgf:relationship"/>
	<!-- /cbgf:rename-rename -->
	<xsl:template match="/cbgf:relationship">
		<cbgf:relationship>
			<xsl:for-each select="*">
				<xsl:element name="cbgf:{local-name()}">
					<xsl:apply-templates select="*"/>
				</xsl:element>
			</xsl:for-each>
		</cbgf:relationship>
	</xsl:template>
	<xsl:template match="bgf:production">
		<bgf:production>
			<xsl:copy-of select="*"/>
		</bgf:production>
	</xsl:template>
	<xsl:template match="bgf:expression">
		<bgf:expression>
			<xsl:copy-of select="*"/>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="nonterminal">
		<nonterminal>
			<xsl:choose>
				<xsl:when test="not(*)">
					<!-- check for renaming? -->
					<xsl:value-of select="."/>
				</xsl:when>
				<xsl:when test="$rnm/cbgf:rename-rename/nonterminal/from = ./from">
					<xsl:variable name="frm" select="./from"/>
					<from>
						<xsl:value-of select="$rnm/cbgf:rename-rename/nonterminal[from=$frm]/to"/>
					</from>
					<xsl:copy-of select="to"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:copy-of select="*"/>
				</xsl:otherwise>
			</xsl:choose>
		</nonterminal>
	</xsl:template>
</xsl:stylesheet>
