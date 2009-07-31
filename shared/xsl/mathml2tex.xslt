<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:ldf="http://planet-sl.org/ldf"
    xmlns:mml="http://www.w3.org/1998/Math/MathML">  
  
  <xsl:template match="mml:mtext">
    <xsl:text>\textrm{</xsl:text>
    <xsl:apply-templates select="node()"/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xsl:template match="mml:mi">
    <xsl:value-of select="."/>
  </xsl:template>
  <xsl:template match="mml:mo">
    <xsl:value-of select="."/>
  </xsl:template>
  <xsl:template match="mml:msup">
    <xsl:apply-templates select="*[1]"/>
    <xsl:text>^</xsl:text>
    <xsl:apply-templates select="*[2]"/>
  </xsl:template>
  <xsl:template match="mml:msub">
    <xsl:apply-templates select="*[1]"/>
    <xsl:text>_</xsl:text>
    <xsl:apply-templates select="*[2]"/>
  </xsl:template>
  <xsl:template match="mml:mrow">
    <xsl:text>{</xsl:text>
    <xsl:apply-templates select="node()"/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xsl:template match="mml:mfenced">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="node()"/>
    <xsl:text>)</xsl:text>
  </xsl:template>
  <xsl:template match="mml:varepsilon">
    <xsl:text>\varepsilon </xsl:text>
  </xsl:template>
  
</xsl:stylesheet>
