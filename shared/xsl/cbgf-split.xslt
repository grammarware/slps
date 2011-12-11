<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:cbgf="http://planet-sl.org/cbgf" xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/cbgf:relationship">
		<cbgf:relationship>
			<xsl:copy-of select="*[position() &lt;= count(/cbgf:relationship/*[todo][1]/preceding-sibling::*)]"/>
		</cbgf:relationship>
	</xsl:template>
</xsl:stylesheet>
