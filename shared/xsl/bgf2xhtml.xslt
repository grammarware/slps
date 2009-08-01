<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:bgf="http://planet-sl.org/bgf"
    xmlns:xhtml="http://www.w3.org/1999/xhtml">

  <xsl:output
       method="html"
       encoding="UTF-8"
       doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN"
       doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
    />
 
  <xsl:template match="/bgf:grammar">
    <xsl:apply-templates select="./bgf:*"/>
  </xsl:template>

  <xsl:template match="bgf:production">
    <xsl:if test="./label">
      <xsl:call-template name="displaylabel">
        <xsl:with-param name="l" select="label"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:call-template name="displaynt">
      <xsl:with-param name="nt" select="nonterminal"/>
    </xsl:call-template>
     <xsl:text>:</xsl:text>
    <xsl:choose>
      <xsl:when test="./bgf:expression/choice">
        <xsl:for-each select="./bgf:expression/choice/bgf:expression">
          <xsl:text>
        </xsl:text>
          <xsl:call-template name="no-parenthesis">
            <xsl:with-param name="expr" select="."/>
          </xsl:call-template>
        </xsl:for-each>
        <xsl:text>
</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>
        </xsl:text>
        <xsl:call-template name="no-parenthesis">
          <xsl:with-param name="expr" select="./bgf:expression"/>
        </xsl:call-template>
        <xsl:text>
</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="bgf:expression">
    <xsl:apply-templates select="*"/>
  </xsl:template>

  <xsl:template match="marked">
    <span xmlns="http://www.w3.org/1999/xhtml" class="marked">
      <xsl:text><![CDATA[<]]></xsl:text>
      <xsl:apply-templates select="./*"/>
      <xsl:text><![CDATA[>]]></xsl:text>
    </span>
  </xsl:template>

  <xsl:template match="plus">
    <xsl:apply-templates select="./*"/>
    <xsl:text>+</xsl:text>
  </xsl:template>

  <xsl:template match="star">
    <xsl:apply-templates select="./*"/>
    <xsl:text>*</xsl:text>
  </xsl:template>

  <xsl:template match="optional">
    <xsl:apply-templates select="./*"/>
    <xsl:text>?</xsl:text>
  </xsl:template>

  <xsl:template match="terminal">
    <xsl:call-template name="displayt">
      <xsl:with-param name="t" select="."/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="value">
    <span xmlns="http://www.w3.org/1999/xhtml" class="meta">
      <xsl:choose>
        <xsl:when test=". = 'string'">
          <xsl:text>str</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>int</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </span>
  </xsl:template>

  <xsl:template match="epsilon">
    <xsl:text>&#x03B5;</xsl:text>
  </xsl:template>

  <xsl:template match="empty">
    <span xmlns="http://www.w3.org/1999/xhtml" class="meta">
      <xsl:text>empty</xsl:text>
    </span>
  </xsl:template>

  <xsl:template match="any">
    <span xmlns="http://www.w3.org/1999/xhtml" class="meta">
      <xsl:text>any</xsl:text>
    </span>  </xsl:template>
  
  <xsl:template match="nonterminal">
    <xsl:call-template name="linknt">
      <xsl:with-param name="nt" select="."/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="selectable">
    <span xmlns="http://www.w3.org/1999/xhtml" class="sel">
      <xsl:value-of select="selector"/>
    </span>
    <xsl:text>::</xsl:text>
    <xsl:choose>
      <xsl:when test="local-name(bgf:expression/*) = 'star'
                   or local-name(bgf:expression/*) = 'optional'
                   or local-name(bgf:expression/*) = 'plus'">
        <xsl:text>(</xsl:text>
        <xsl:apply-templates select="bgf:expression"/>
        <xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="bgf:expression"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="sequence">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="./bgf:expression[1]/*"/>
    <xsl:for-each select="./bgf:expression[position()>1]">
      <xsl:text> </xsl:text>
      <xsl:apply-templates select="./*"/>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <!-- inner choices - BNF bar -->
  <xsl:template match="choice">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="./bgf:expression[1]/*"/>
    <xsl:for-each select="./bgf:expression[position()>1]">
      <xsl:text> | </xsl:text>
      <xsl:apply-templates select="./*"/>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template name="no-parenthesis">
    <xsl:param name="expr"/>
    <xsl:choose>
      <xsl:when test="$expr/sequence">
        <xsl:apply-templates select="$expr/sequence/bgf:expression[1]/*"/>
        <xsl:for-each select="$expr/sequence/bgf:expression[position()>1]">
          <xsl:text> </xsl:text>
          <xsl:apply-templates select="./*"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="$expr"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="linknt">
    <xsl:param name="nt"/>
    <xsl:element name="a" namespace="http://www.w3.org/1999/xhtml">
      <xsl:attribute name="href">
        <xsl:text>#</xsl:text>
        <xsl:value-of select="$nt"/>
      </xsl:attribute>
      <span class="nt">
        <xsl:value-of select="$nt"/>
      </span>
    </xsl:element>
  </xsl:template>

  <xsl:template name="displaynt">
    <xsl:param name="nt"/>
    <xsl:element name="a" namespace="http://www.w3.org/1999/xhtml">
      <xsl:attribute name="name">
        <xsl:value-of select="$nt"/>
      </xsl:attribute>
      <span class="nt">
        <xsl:value-of select="$nt"/>
      </span>
    </xsl:element>
  </xsl:template>

  <xsl:template name="displayt">
    <xsl:param name="t"/>
    <span xmlns="http://www.w3.org/1999/xhtml" class="t">
      <xsl:text>"</xsl:text>
      <xsl:value-of select="$t"/>
      <xsl:text>"</xsl:text>
    </span>
  </xsl:template>

  <xsl:template name="displaylabel">
    <xsl:param name="l"/>
    <xsl:text>[</xsl:text>
    <xsl:element name="a" namespace="http://www.w3.org/1999/xhtml">
      <xsl:attribute name="class">
        <xsl:text>label</xsl:text>
      </xsl:attribute>
      <xsl:attribute name="name">
        <xsl:value-of select="$l"/>
      </xsl:attribute>
      <xsl:value-of select="$l" />
    </xsl:element>
    <xsl:text>] </xsl:text>
  </xsl:template>

</xsl:stylesheet>
