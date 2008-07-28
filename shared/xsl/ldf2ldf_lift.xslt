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

  <xsl:template match="text">
    <xsl:choose>
      <xsl:when test="./ldf:sample">
        <xsl:call-template name="processastext">
          <xsl:with-param name="atr" select="./@*" />
          <xsl:with-param name="txt" select="./ldf:sample/preceding-sibling::node()" />
        </xsl:call-template>
        <xsl:element name="sample">
          <xsl:copy-of select="./ldf:sample/@*"/>
          <xsl:value-of select="./ldf:sample" />
        </xsl:element>
        <xsl:call-template name="processastext">
          <xsl:with-param name="atr" select="./@*"/>
          <xsl:with-param name="txt" select="./ldf:sample/following-sibling::node()" />
        </xsl:call-template>
      </xsl:when>

      <xsl:when test="./ldf:runnable">
        <xsl:call-template name="processastext">
          <xsl:with-param name="atr" select="./@*" />
          <xsl:with-param name="txt" select="./ldf:runnable/preceding-sibling::node()" />
        </xsl:call-template>
        <xsl:element name="runnable">
          <xsl:copy-of select="./ldf:runnable/@*"/>
          <xsl:value-of select="./ldf:runnable"/>
        </xsl:element>
        <xsl:call-template name="processastext">
          <xsl:with-param name="atr" select="./@*"/>
          <xsl:with-param name="txt" select="./ldf:runnable/following-sibling::node()" />
        </xsl:call-template>
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
