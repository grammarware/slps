<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:bgf="http://planet-sl.org/bgf"
    xmlns:ldf="http://planet-sl.org/ldf"
    xmlns:ldx="http://planet-sl.org/ldx"
    xmlns:html="http://www.w3.org/1999/xhtml">

  <xsl:template match="*">
    <xsl:element name="{name(.)}">
      <xsl:copy-of select="./@*"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <xsl:template name="processastext">
    <xsl:param name="atr" />
    <xsl:param name="txt" />
    <xsl:element name="text">
      <xsl:copy-of select="$atr"/>
      <xsl:apply-templates select="$txt"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="content">
    <xsl:choose>
      <xsl:when test="./text/ldx:tail">
        <content>
          <xsl:call-template name="processastext">
            <xsl:with-param name="atr" select="./text/@*" />
            <xsl:with-param name="txt" select="./text/ldx:tail/preceding-sibling::node()" />
          </xsl:call-template>
          <xsl:apply-templates select="./grammar" />
          <xsl:call-template name="processastext">
            <xsl:with-param name="atr" select="./text/@*"/>
            <xsl:with-param name="txt" select="./text/ldx:tail/node()" />
          </xsl:call-template>
        </content>
      </xsl:when>

      <xsl:otherwise>
        <xsl:element name="{name(.)}">
          <xsl:copy-of select="./@*"/>
          <xsl:apply-templates/>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
