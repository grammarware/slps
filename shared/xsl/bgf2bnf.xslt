<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:bgf="http://planet-sl.org/bgf"
    xmlns:xhtml="http://www.w3.org/1999/xhtml">

  <xsl:output
      method="text"
      encoding="UTF-8"
      omit-xml-declaration="yes"
      />

 <xsl:template match="/bgf:grammar">
      <xsl:apply-templates select="./bgf:*"/>
  </xsl:template>

  <xsl:template match="bgf:production">
    <xsl:value-of select="./nonterminal"/>
    <xsl:text>:</xsl:text>
    <xsl:choose>
      <xsl:when test="./bgf:expression/choice">
        <xsl:for-each select="./bgf:expression/choice/bgf:expression">
          <xsl:text>
        </xsl:text>
          <xsl:apply-templates select="."/>
        </xsl:for-each>
        <xsl:text>
</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>
        </xsl:text>
        <xsl:apply-templates select="./bgf:expression"/>
        <xsl:text>
</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="bgf:expression">
    <xsl:apply-templates select="./*"/>
  </xsl:template>
  
  <xsl:template match="plus">
    <xsl:text>( </xsl:text>
    <xsl:apply-templates select="./*"/>
    <xsl:text>)+ </xsl:text>
  </xsl:template>
  
  <xsl:template match="star">
    <xsl:text>( </xsl:text>
    <xsl:apply-templates select="./*"/>
    <xsl:text>)* </xsl:text>
  </xsl:template>
  
  <xsl:template match="optional">
    <xsl:text>[ </xsl:text>
    <xsl:apply-templates select="./*"/>
    <xsl:text>] </xsl:text>
  </xsl:template>
  
  <xsl:template match="terminal">
    <xsl:text>"</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>" </xsl:text>
  </xsl:template>
  
  <xsl:template match="value">
    <xsl:choose>
      <xsl:when test=". = 'string'">
        <xsl:text>STRING </xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>INT </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="epsilon">
    <xsl:text>EPSILON </xsl:text>
  </xsl:template>

  <xsl:template match="empty">
    <xsl:text>EMPTY </xsl:text>
  </xsl:template>

  <xsl:template match="any">
    <xsl:text>ANY </xsl:text>
  </xsl:template>

  <xsl:template match="nonterminal">
    <xsl:value-of select="."/>
    <xsl:text> </xsl:text>
  </xsl:template>
  
  <xsl:template match="sequence">
    <xsl:apply-templates select="./*"/>
  </xsl:template>

  <!-- top level choices - one per line -->
  <xsl:template match="/bgf:grammar/bgf:production/bgf:expression/choice">
    <xsl:call-template name="car">
      <xsl:with-param name="expr" select="./bgf:expression[1]"/>
    </xsl:call-template>
    <xsl:for-each select="./bgf:expression[position()>1]">
      <xsl:call-template name="cdr">
        <xsl:with-param name="expr" select="."/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <!-- inner choices - BNF bar -->
  <xsl:template match="choice">
    <xsl:text>( </xsl:text>
    <xsl:apply-templates select="./bgf:expression[1]/*"/>
    <xsl:for-each select="./bgf:expression[position()>1]">
      <xsl:text>| </xsl:text>
      <xsl:apply-templates select="./*"/>
    </xsl:for-each>
    <xsl:text>) </xsl:text>
  </xsl:template>


  <xsl:template name="car">
    <xsl:param name="expr"/>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="$expr/*"/>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template name="cdr">
    <xsl:param name="expr"/>
    <xsl:text>
        </xsl:text>
    <xsl:apply-templates select="$expr/*"/>
    <xsl:text>
</xsl:text>
  </xsl:template>

</xsl:stylesheet>
