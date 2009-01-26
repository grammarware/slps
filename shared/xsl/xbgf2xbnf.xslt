<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:bgf="http://planet-sl.org/bgf"
    xmlns:xbgf="http://planet-sl.org/xbgf"
    xmlns:xhtml="http://www.w3.org/1999/xhtml">

  <xsl:import href="bgf2bnf.xslt" />
  
  <xsl:output
      method="text"
      encoding="UTF-8"
      omit-xml-declaration="yes"
      />

 <xsl:template match="/xbgf:sequence">
      <xsl:apply-templates select="./xbgf:*"/>
  </xsl:template>
  
  <!-- optional context -->
  <xsl:template name="context">
    <xsl:param name="in"/>
    <xsl:choose>
      <xsl:when test="$in/nonterminal">
        <xsl:text> in </xsl:text>
        <xsl:value-of select="$in/nonterminal" />
      </xsl:when>
      <xsl:when test="$in/label">
        <xsl:text> in [</xsl:text>
        <xsl:value-of select="$in/label" />
        <xsl:text>]</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <!-- chain, define, ... -->

  <xsl:template match="xbgf:*">
    <xsl:value-of select="local-name()" />
    <xsl:text>(
 </xsl:text>
    <xsl:apply-templates select="./bgf:production"/>
    <xsl:call-template name="context">
      <xsl:with-param name="in" select="./in"/>
    </xsl:call-template>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:add">
    <xsl:value-of select="local-name()" />
    <xsl:text>(
 </xsl:text>
    <xsl:choose>
      <xsl:when test="./bgf:production">
        <xsl:apply-templates select="./bgf:production"/>
        <xsl:call-template name="context">
          <xsl:with-param name="in" select="./in"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="./bgf:expression">
        <xsl:apply-templates select="./bgf:expression[1]"/>
        <xsl:text>,
 </xsl:text>
        <xsl:apply-templates select="./bgf:expression[2]"/>
        <xsl:choose>
          <xsl:when test="in">
            <xsl:text>
</xsl:text>
            <xsl:call-template name="context">
              <xsl:with-param name="in" select="./in"/>
            </xsl:call-template>
          </xsl:when>
        </xsl:choose>
      </xsl:when>
    </xsl:choose>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <!--xsl:template match="xbgf:designate">
    <xsl:value-of select="local-name()" />
    <xsl:text>("</xsl:text>
    <xsl:value-of select="./bgf:production/label" />
    <xsl:text>",
 </xsl:text>
    <xsl:apply-templates select="./bgf:production"/>
    <xsl:text>);
</xsl:text>
  </xsl:template-->

  <xsl:template match="xbgf:deyaccify|xbgf:eliminate|xbgf:horizontal|xbgf:inline|xbgf:undefine">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:distribute|xbgf:vertical">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:call-template name="context">
      <xsl:with-param name="in" select="."/>
    </xsl:call-template>
    <xsl:text> );
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:factor|xbgf:massage|xbgf:narrow|xbgf:replace|xbgf:widen">
    <xsl:value-of select="local-name()" />
    <xsl:text>(
 </xsl:text>
    <xsl:apply-templates select="./bgf:expression[1]"/>
    <xsl:text>,
 </xsl:text>
    <xsl:apply-templates select="./bgf:expression[2]"/>
    <xsl:choose>
      <xsl:when test="in">
        <xsl:text>
</xsl:text>
        <xsl:call-template name="context">
          <xsl:with-param name="in" select="./in"/>
        </xsl:call-template>
      </xsl:when>
    </xsl:choose>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:fold|xbgf:unfold">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:value-of select="./nonterminal"/>
    <xsl:choose>
      <xsl:when test="in">
        <xsl:text>
</xsl:text>
        <xsl:call-template name="context">
          <xsl:with-param name="in" select="./in"/>
        </xsl:call-template>
      </xsl:when>
    </xsl:choose>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:rename">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:value-of select="local-name(*)" />
    <xsl:text>
 </xsl:text>
    <xsl:apply-templates select="./*/*[1]"/>
    <xsl:text>,
 </xsl:text>
    <xsl:apply-templates select="./*/*[2]"/>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:reroot">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:value-of select="./root"/>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:strip">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:choose>
      <xsl:when test="label">
        <xsl:text>[</xsl:text>
        <xsl:value-of select="*/text()" />
        <xsl:text>]</xsl:text>
      </xsl:when>
      <xsl:when test="selector">
        <xsl:value-of select="*/text()" />
        <xsl:text>::</xsl:text>
      </xsl:when>
      <xsl:when test="terminal">
        <xsl:value-of select="local-name(*)" />
        <xsl:text>, "</xsl:text>
        <xsl:value-of select="*/text()" />
        <xsl:text>"</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="local-name(*)" />
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:unite">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="add/text()"/>
    <xsl:text>, </xsl:text>
    <xsl:apply-templates select="to/text()"/>
    <xsl:text>);
</xsl:text>
  </xsl:template>

</xsl:stylesheet>
