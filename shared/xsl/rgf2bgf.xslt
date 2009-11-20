<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:rgf="http://planet-sl.org/rgf">

  <xsl:output method="xml" encoding="UTF-8"/>

  <xsl:template match="/rgf:grammar">
    <xsl:for-each select="entry">
      <xsl:choose>
        <xsl:when test="local-name() = 'name'">
        </xsl:when>
      </xsl:choose>
      <xsl:if test="local-name() = 'terminator-symbol'">
        <xsl:value-of select="position()"/>
        <xsl:text>
</xsl:text>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>



</xsl:stylesheet>