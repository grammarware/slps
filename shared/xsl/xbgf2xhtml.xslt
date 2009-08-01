<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:bgf="http://planet-sl.org/bgf"
    xmlns:xbgf="http://planet-sl.org/xbgf"
    xmlns:xhtml="http://www.w3.org/1999/xhtml">

  <xsl:import href="bgf2xhtml.xslt" />

  <xsl:output
      method="html"
      encoding="UTF-8"
      doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN"
      doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
    />

  <xsl:template match="/xbgf:sequence">
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
      <body>
        <xsl:apply-templates select="./xbgf:*"/>
      </body>
    </html>
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
        <span xmlns="http://www.w3.org/1999/xhtml" class="meta">
          <xsl:text> in </xsl:text>
        </span>
        <xsl:call-template name="linknt">
          <xsl:with-param name="nt" select="$in/nonterminal"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$in/label">
        <span xmlns="http://www.w3.org/1999/xhtml" class="meta">
          <xsl:text> in </xsl:text>
        </span>
        <xsl:call-template name="linklabel">
          <xsl:with-param name="l" select="$in/label"/>
        </xsl:call-template>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <!-- chain, define, ... -->

  <xsl:template match="xbgf:*">
    <xsl:call-template name="linkcmd">
      <xsl:with-param name="cmd" select="local-name()"/>
    </xsl:call-template>
    <xsl:text>
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

  <xsl:template match="xbgf:add|xbgf:remove">
    <xsl:call-template name="linkcmd">
      <xsl:with-param name="cmd">
        <xsl:choose>
          <xsl:when test="local-name() = 'add' and ./vertical">
            <xsl:text>addV</xsl:text>
          </xsl:when>
          <xsl:when test="local-name() = 'add' and ./horizontal">
            <xsl:text>addH</xsl:text>
          </xsl:when>
          <xsl:when test="local-name() = 'remove' and ./vertical">
            <xsl:text>removeV</xsl:text>
          </xsl:when>
          <xsl:when test="local-name() = 'remove' and ./horizontal">
            <xsl:text>removeH</xsl:text>
          </xsl:when>
        </xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:text>
 </xsl:text>
    <xsl:apply-templates select="./*/bgf:production"/>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:deyaccify|xbgf:eliminate|xbgf:horizontal|xbgf:inline|xbgf:undefine">
    <xsl:call-template name="linkcmd">
      <xsl:with-param name="cmd" select="local-name()"/>
    </xsl:call-template>
    <xsl:call-template name="linknt">
      <xsl:with-param name="nt" select="text()"/>
    </xsl:call-template>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:unlabel">
    <xsl:call-template name="linkcmd">
      <xsl:with-param name="cmd" select="local-name()"/>
    </xsl:call-template>
    <xsl:call-template name="linklabel">
      <xsl:with-param name="l" select="label"/>
    </xsl:call-template>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:distribute|xbgf:vertical">
    <xsl:element name="a" namespace="http://www.w3.org/1999/xhtml">
      <xsl:attribute name="class">
        <xsl:text>cmd</xsl:text>
      </xsl:attribute>
      <xsl:attribute name="href">
        <xsl:text>#</xsl:text>
        <xsl:value-of select="local-name()"/>
      </xsl:attribute>
      <xsl:value-of select="local-name()" />
    </xsl:element> <xsl:text>(</xsl:text>
    <xsl:call-template name="context">
      <xsl:with-param name="in" select="."/>
    </xsl:call-template>
    <xsl:text> );
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:factor|xbgf:massage|xbgf:narrow|xbgf:replace|xbgf:widen">
    <xsl:call-template name="linkcmd">
      <xsl:with-param name="cmd" select="local-name()"/>
    </xsl:call-template>
    <xsl:text>
 </xsl:text>
    <xsl:apply-templates select="bgf:expression[1]"/>
    <xsl:text>,
 </xsl:text>
    <xsl:apply-templates select="bgf:expression[2]"/>
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
    <xsl:call-template name="linkcmd">
      <xsl:with-param name="cmd" select="local-name()"/>
    </xsl:call-template>
    <xsl:call-template name="linknt">
      <xsl:with-param name="nt" select="./nonterminal"/>
    </xsl:call-template>
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
        <xsl:call-template name="linkcmd">
          <xsl:with-param name="cmd">
            <xsl:text>renameL</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="linklabel">
          <xsl:with-param name="l" select="label/from"/>
        </xsl:call-template>
        <xsl:text>, </xsl:text>
        <xsl:call-template name="displaylabel">
          <xsl:with-param name="l" select="label/to"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="local-name(*) = 'nonterminal'">
        <xsl:call-template name="linkcmd">
          <xsl:with-param name="cmd">
            <xsl:text>renameN</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="linknt">
          <xsl:with-param name="nt" select="nonterminal/from"/>
        </xsl:call-template>
        <xsl:text>, </xsl:text>
        <xsl:call-template name="displaynt">
          <xsl:with-param name="nt" select="nonterminal/to"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="local-name(*) = 'selector'">
        <xsl:call-template name="linkcmd">
          <xsl:with-param name="cmd">
            <xsl:text>renameS</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
        <span xmlns="http://www.w3.org/1999/xhtml" class="sel">
          <xsl:apply-templates select="selector/from"/>
        </span>
        <xsl:text>, </xsl:text>
        <span xmlns="http://www.w3.org/1999/xhtml" class="sel">
          <xsl:apply-templates select="selector/to"/>
        </span>
      </xsl:when>
      <xsl:when test="local-name(*) = 'terminal'">
        <xsl:call-template name="linkcmd">
          <xsl:with-param name="cmd">
            <xsl:text>renameT</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="displayt">
          <xsl:with-param name="t" select="terminal/from"/>
        </xsl:call-template>
        <xsl:text>, </xsl:text>
        <xsl:call-template name="displayt">
          <xsl:with-param name="t" select="terminal/to"/>
        </xsl:call-template>
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
    <xsl:call-template name="linkcmd">
      <xsl:with-param name="cmd" select="local-name()"/>
    </xsl:call-template>
    <xsl:call-template name="linknt">
      <xsl:with-param name="nt" select="./root[1]"/>
    </xsl:call-template>
    <xsl:for-each select="./root[position()>1]">
      <xsl:text>, </xsl:text>
      <xsl:call-template name="linknt">
        <xsl:with-param name="nt" select="."/>
      </xsl:call-template>
    </xsl:for-each>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template match="xbgf:strip">
    <xsl:call-template name="linkcmd">
      <xsl:with-param name="cmd" select="local-name()"/>
    </xsl:call-template>
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
    <xsl:call-template name="linkcmd">
      <xsl:with-param name="cmd" select="local-name()"/>
    </xsl:call-template>
    <xsl:call-template name="linknt">
      <xsl:with-param name="nt" select="add/text()"/>
    </xsl:call-template>
    <xsl:text>, </xsl:text>
    <xsl:call-template name="linknt">
      <xsl:with-param name="nt" select="to/text()"/>
    </xsl:call-template>
    <xsl:text>);
</xsl:text>
  </xsl:template>

  <xsl:template name="linkcmd">
    <xsl:param name="cmd"/>
    <xsl:element name="a" namespace="http://www.w3.org/1999/xhtml">
      <xsl:attribute name="class">
        <xsl:text>cmd</xsl:text>
      </xsl:attribute>
      <xsl:attribute name="href">
        <xsl:text>#</xsl:text>
        <xsl:value-of select="$cmd"/>
      </xsl:attribute>
      <xsl:value-of select="$cmd" />
    </xsl:element>
    <xsl:text>(</xsl:text>
  </xsl:template>

  <xsl:template name="linklabel">
    <xsl:param name="l"/>
    <xsl:text>[</xsl:text>
    <xsl:element name="a" namespace="http://www.w3.org/1999/xhtml">
      <xsl:attribute name="class">
        <xsl:text>label</xsl:text>
      </xsl:attribute>
      <xsl:attribute name="href">
        <xsl:text>#</xsl:text>
        <xsl:value-of select="$l"/>
      </xsl:attribute>
      <xsl:value-of select="$l" />
    </xsl:element>
    <xsl:text>]</xsl:text>
  </xsl:template>

</xsl:stylesheet>
