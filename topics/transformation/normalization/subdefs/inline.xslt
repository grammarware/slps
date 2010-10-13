<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xbgf="http://planet-sl.org/xbgf" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/bgf:grammar">
		<xbgf:sequence xmlns:bgf="http://planet-sl.org/bgf" xmlns:xbgf="http://planet-sl.org/xbgf">
			<xsl:for-each select="bgf:production">
				<xsl:if test="contains(nonterminal,'_grp_')">
					<xbgf:inline>
						<xsl:value-of select="nonterminal"/>
					</xbgf:inline>
				</xsl:if>
			</xsl:for-each>
		</xbgf:sequence>
	</xsl:template>
</xsl:stylesheet>
