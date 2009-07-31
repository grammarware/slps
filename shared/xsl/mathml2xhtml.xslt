<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:ldf="http://planet-sl.org/ldf"
    xmlns:mml="http://www.w3.org/1998/Math/MathML"
    xmlns:xhtml="http://www.w3.org/1999/xhtml">

  <xsl:template match="mml:mtext">
    <xsl:apply-templates select="node()"/>
  </xsl:template>
  <xsl:template match="mml:mi">
    <em xmlns="http://www.w3.org/1999/xhtml">
      <xsl:value-of select="."/>
    </em>
  </xsl:template>
  <xsl:template match="mml:mo">
    <xsl:value-of select="."/>
  </xsl:template>
  <xsl:template match="mml:msup">
    <xsl:apply-templates select="*[1]"/>
    <sup xmlns="http://www.w3.org/1999/xhtml">
      <xsl:apply-templates select="*[2]"/>
    </sup>
  </xsl:template>
  <xsl:template match="mml:msub">
    <xsl:apply-templates select="*[1]"/>
    <sub xmlns="http://www.w3.org/1999/xhtml">
      <xsl:apply-templates select="*[2]"/>
    </sub>
  </xsl:template>
  <xsl:template match="mml:mrow">
    <xsl:apply-templates select="node()"/>
  </xsl:template>
  <xsl:template match="mml:mfenced">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="node()"/>
    <xsl:text>)</xsl:text>
  </xsl:template>
  <xsl:template match="mml:varepsilon">
    <xsl:text>&#x03B5;</xsl:text>
  </xsl:template>

</xsl:stylesheet>
