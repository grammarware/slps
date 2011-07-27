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

  <xsl:template match="xbgf:atomic">
    <xsl:text>[[
</xsl:text>
    <xsl:apply-templates select="./xbgf:*"/>
    <xsl:text>]];
</xsl:text>
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
    <xsl:for-each select="./bgf:production">
      <xsl:text> </xsl:text>
      <xsl:apply-templates select="."/>
    </xsl:for-each>
    <xsl:call-template name="context">
      <xsl:with-param name="in" select="./in"/>
    </xsl:call-template>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:add">
    <xsl:choose>
      <xsl:when test="./vertical">
        <xsl:text>addV(
 </xsl:text>
        <xsl:apply-templates select="./vertical/bgf:production"/>
      </xsl:when>
      <xsl:when test="./horizontal">
        <xsl:text>addH(
 </xsl:text>
        <xsl:apply-templates select="./horizontal/bgf:production"/>
      </xsl:when>
    </xsl:choose>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:remove">
    <xsl:choose>
      <xsl:when test="./vertical">
        <xsl:text>removeV(
 </xsl:text>
        <xsl:apply-templates select="./vertical/bgf:production"/>
      </xsl:when>
      <xsl:when test="./horizontal">
        <xsl:text>removeH(
 </xsl:text>
        <xsl:apply-templates select="./horizontal/bgf:production"/>
      </xsl:when>
    </xsl:choose>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:undefine">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:value-of select="*[1]/text()"/>
    <xsl:for-each select="*[position()>1]">
      <xsl:text>, </xsl:text>
      <xsl:value-of select="text()"/>
    </xsl:for-each>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:inline">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:eliminate|xbgf:vertical|xbgf:horizontal">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:value-of select="nonterminal"/>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:unlabel">
    <xsl:value-of select="local-name()" />
    <xsl:text>([</xsl:text>
    <xsl:value-of select="label"/>
    <xsl:text>]);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:distribute|xbgf:vertical|xbgf:horizontal">
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
    <xsl:call-template name="no-parenthesis">
      <xsl:with-param name="expr" select="./bgf:expression[1]"/>
    </xsl:call-template>
    <xsl:text>,
 </xsl:text>
    <xsl:call-template name="no-parenthesis">
      <xsl:with-param name="expr" select="./bgf:expression[2]"/>
    </xsl:call-template>
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

  <xsl:template match="xbgf:deyaccify|xbgf:fold|xbgf:unfold">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:value-of select="./nonterminal"/>
    <xsl:choose>
      <xsl:when test="in">
        <xsl:call-template name="context">
          <xsl:with-param name="in" select="./in"/>
        </xsl:call-template>
      </xsl:when>
    </xsl:choose>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:rename">
    <xsl:choose>
      <xsl:when test="local-name(*) = 'label'">
        <xsl:text>renameL([</xsl:text>
        <xsl:value-of select="label/from"/>
        <xsl:text>], [</xsl:text>
        <xsl:value-of select="label/to"/>
        <xsl:text>]</xsl:text>
      </xsl:when>
      <xsl:when test="local-name(*) = 'nonterminal'">
        <xsl:text>renameN(</xsl:text>
        <xsl:value-of select="nonterminal/from"/>
        <xsl:text>, </xsl:text>
        <xsl:value-of select="nonterminal/to"/>
      </xsl:when>
      <xsl:when test="local-name(*) = 'selector'">
        <xsl:text>renameS(</xsl:text>
        <xsl:value-of select="selector/from"/>
        <xsl:text>, </xsl:text>
        <xsl:value-of select="selector/to"/>
      </xsl:when>
      <xsl:when test="local-name(*) = 'terminal'">
        <xsl:text>renameT("</xsl:text>
        <xsl:value-of select="terminal/from"/>
        <xsl:text>", "</xsl:text>
        <xsl:value-of select="terminal/to"/>
        <xsl:text>"</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="local-name()" />
        <xsl:text>-</xsl:text>
        <xsl:value-of select="local-name(*)" />
        <xsl:text>(</xsl:text>
        <xsl:apply-templates select="./*/*[1]"/>
        <xsl:text>, </xsl:text>
        <xsl:apply-templates select="./*/*[2]"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:reroot">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:value-of select="./root[1]"/>
    <xsl:for-each select="./root[position()>1]">
      <xsl:text>, </xsl:text>
      <xsl:value-of select="."/>
    </xsl:for-each>
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

  <xsl:template match="xbgf:equate">
    <xsl:value-of select="local-name()" />
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="align/text()"/>
    <xsl:text>, </xsl:text>
    <xsl:apply-templates select="with/text()"/>
    <xsl:text>);
</xsl:text>
  </xsl:template>

</xsl:stylesheet>
