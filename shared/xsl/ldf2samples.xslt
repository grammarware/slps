<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:bgf="http://planet-sl.org/bgf"
    xmlns:ldf="http://planet-sl.org/ldf">
  <xsl:output method="xml" encoding="UTF-8"/>

  <xsl:template match="/ldf:document">
      <ldf:document xmlns:ldf="http://planet-sl.org/ldf">
         <xsl:apply-templates select="content"/>
      </ldf:document>
  </xsl:template>

  <xsl:template match="sample">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <xsl:template match="runnable">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="text"/>
  <xsl:template match="title"/>
  <xsl:template match="grammar"/>

</xsl:stylesheet>
