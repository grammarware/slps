<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:bgf="http://planet-sl.org/bgf"
>
  <xsl:output method="xml" encoding="UTF-8"/>

  <xsl:template match="/program">
    <bgf:grammar>
      <xsl:apply-templates select="repeat_statement/statement/defineStatement"/>
    </bgf:grammar>
  </xsl:template>

  <xsl:template match="defineStatement">
    <bgf:production>
      <nonterminal>
        <xsl:value-of select="typeid/id" />
      </nonterminal>
      <!--xsl:apply-templates select="repeat_literalOrType/literalOrType/type/typeSpec"/-->
      <xsl:choose>
        <xsl:when test="repeat_barLiteralsAndTypes">
          <bgf:expression>
            <choice>
              <xsl:call-template name="sequenceOrNot">
                <xsl:with-param name="list" select="repeat_literalOrType"/>
              </xsl:call-template>
              <xsl:for-each select="repeat_barLiteralsAndTypes/barLiteralsAndTypes/repeat_literalOrType">
                <xsl:call-template name="sequenceOrNot">
                  <xsl:with-param name="list" select="."/>
                </xsl:call-template>
              </xsl:for-each>
            </choice>
          </bgf:expression>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="sequenceOrNot">
            <xsl:with-param name="list" select="repeat_literalOrType"/>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </bgf:production>
  </xsl:template>

  <xsl:template name="sequenceOrNot">
    <xsl:param name="list"/>
    <bgf:expression>
      <xsl:choose>
        <xsl:when test="count($list/literalOrType)=1">
          <xsl:apply-templates select="$list/literalOrType"/>
        </xsl:when>
        <xsl:otherwise>
          <sequence>
            <xsl:apply-templates select="$list/literalOrType"/>
          </sequence>
        </xsl:otherwise>
      </xsl:choose>
    </bgf:expression>
  </xsl:template>

  <xsl:template match="literalOrType">
    <!--xsl:for-each select="repeat_literalOrType/literalOrType"-->
    <xsl:choose>
      <xsl:when test="type/typeSpec/opt_typeRepeater/typeRepeater='+'">
        <bgf:expression>
          <plus>
            <nonterminal>
              <xsl:value-of select="type/typeSpec/typeid/id" />
            </nonterminal>
          </plus>
        </bgf:expression>
      </xsl:when>
      <xsl:when test="type/typeSpec/opt_typeRepeater/typeRepeater='*'">
        <bgf:expression>
          <star>
            <nonterminal>
              <xsl:value-of select="type/typeSpec/typeid/id" />
            </nonterminal>
          </star>
        </bgf:expression>
      </xsl:when>
      <xsl:when test="literal">
        <bgf:expression>
          <terminal>
            <xsl:value-of select="literal/unquotedLiteral/special" />
          </terminal>
        </bgf:expression>
      </xsl:when>
      <!-- fallback -->
      <xsl:otherwise>
        <bgf:expression>
          <nonterminal>
            <xsl:value-of select="type/typeSpec/typeid/id" />
          </nonterminal>
        </bgf:expression>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>