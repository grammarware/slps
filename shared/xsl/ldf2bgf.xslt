<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:bgf="http://planet-sl.org/bgf"
    xmlns:ldf="http://planet-sl.org/ldf">
  <xsl:output method="xml" encoding="UTF-8"/>

  <xsl:template match="/ldf:document">
    <bgf:grammar>
      <xsl:apply-templates select="content"/>
    </bgf:grammar>
  </xsl:template>

  <xsl:template match="grammar">
    <xsl:copy-of select="node()"/>
  </xsl:template>

  <xsl:template match="text"/>
  <xsl:template match="title"/>
  <xsl:template match="sample"/>
  <xsl:template match="runnable"/>

</xsl:stylesheet>