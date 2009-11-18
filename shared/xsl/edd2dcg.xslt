<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:edd="http://planet-sl.org/edd"
    xmlns:xhtml="http://www.w3.org/1999/xhtml">

  <xsl:output
      method="text"
      encoding="UTF-8"
      omit-xml-declaration="yes"
      />
  
  <!--
      module Main
    exports context-free start-symbols
    <xsl:apply-templates select="./root" />
    context-free sorts
    <xsl:apply-templates select="./bgf:production" />

    context-free syntax
    <xsl:apply-templates select="./bgf:production" />
-->

  <xsl:template match="/edd:dialect">
    <xsl:text>grammar(g(Ps)) --> productions(Ps).
productions([H|T]) --> production(H), productions(T).
productions([A]) --> production(A).
</xsl:text>
    <xsl:choose>
      <xsl:when test="production/reverse-order">
        <xsl:text>production(p(NT,As)) --> alternatives(As), definingsymbol, name(NT), terminatorsymbol.</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>production(p(NT,As)) --> name(NT), definingsymbol, alternatives(As).</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>
symbols([H|T]) --> symbol(H), string(" "), symbols(T).
symbols([S]) --> symbol(S).
symbol(nt(N)) --> name(N).
</xsl:text>
    <xsl:if test="production/separator">
      <!-- separator symbol -->
      <xsl:call-template name="symbol2definition">
        <xsl:with-param name="predicate">
          <xsl:text>separatorsymbol</xsl:text>
        </xsl:with-param>
        <xsl:with-param name="symbol" select="production/separator"/>
      </xsl:call-template>
      <xsl:text>alternatives([A|T]) --> symbols(A), separatorsymbol, alternatives(T).</xsl:text>
    </xsl:if>
    <xsl:text>
alternatives([S]) --> symbols(S), terminatorsymbol.
</xsl:text>
    <!-- defining symbol -->
    <xsl:call-template name="symbol2definition">
      <xsl:with-param name="predicate">
        <xsl:text>definingsymbol</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="symbol" select="production/defining"/>
    </xsl:call-template>
    <!-- terminator symbol -->
    <xsl:call-template name="symbol2definition">
      <xsl:with-param name="predicate">
        <xsl:text>terminatorsymbol</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="symbol" select="production/terminator"/>
    </xsl:call-template>
    <!-- grouping symbols -->
    <xsl:if test="fragment/group">
      <xsl:call-template name="symbol2definition">
        <xsl:with-param name="predicate">
          <xsl:text>groupstart</xsl:text>
        </xsl:with-param>
        <xsl:with-param name="symbol" select="fragment/group/start"/>
      </xsl:call-template>
      <xsl:call-template name="symbol2definition">
        <xsl:with-param name="predicate">
          <xsl:text>groupend</xsl:text>
        </xsl:with-param>
        <xsl:with-param name="symbol" select="fragment/group/end"/>
      </xsl:call-template>
      <xsl:text>
symbol(br(S)) --> groupstart, symbolchoices(S), groupend.
symbolchoices([H|T]) --> symbols(H), choicesymbol, symbolchoices(T).
symbolchoices([S]) --> symbols(S).
</xsl:text>
    </xsl:if>
    <xsl:if test="fragment/choice-symbol">
      <xsl:call-template name="symbol2definition">
        <xsl:with-param name="predicate">
          <xsl:text>choicesymbol</xsl:text>
        </xsl:with-param>
        <xsl:with-param name="symbol" select="fragment/choice-symbol"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="optionality/symbol">
      <xsl:call-template name="symbol2definition">
        <xsl:with-param name="predicate">
          <xsl:text>optionalitysymbol</xsl:text>
        </xsl:with-param>
        <xsl:with-param name="symbol" select="optionality/symbol"/>
      </xsl:call-template>
      <xsl:text>symbol(opt(nt(N))) --> name(N), optionalitysymbol.
symbol(opt(br(S))) --> groupstart, symbolchoices(S), groupend, optionalitysymbol.
</xsl:text>
    </xsl:if>
    <!-- not implemented: optionality pair -->
    <!-- repetitions -->
    <xsl:for-each select="repetition">
      <xsl:call-template name="symbol2definition">
        <xsl:with-param name="predicate">
          <xsl:text>repetition</xsl:text>
          <xsl:value-of select="name"/>
        </xsl:with-param>
        <xsl:with-param name="symbol" select="markup/symbol"/>
      </xsl:call-template>
      <xsl:text>
symbol(</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>(nt(N))) --> name(N), repetition</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>.
symbol(</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>(br(S))) --> groupstart, symbolchoices(S), groupend, repetition</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>.
</xsl:text>
    </xsl:for-each>
    <xsl:text>
newline --> [10].
layout --> [0' ], layout.
layout --> [0'	], layout.
layout --> [].
keyword(X) --> layout, string(X).
string([],X,X).
string([H|T1],[H|T2],X) :- string(T1,T2,X).
name(V) --> layout, letter(H), letters(T), { atom_chars(V,[H|T]) }.
letters([H|T]) --> letter(H), letters(T).
letters([]) --> [].
<![CDATA[letter(H,[H|T],T) :- H >= 0'a, H =< 0'z.
letter(H,[H|T],T) :- H >= 0'A, H =< 0'Z.]]>
letter(0'-,[0'-|T],T).
</xsl:text>
  </xsl:template>

  <xsl:template name="transformSymbol">
    <xsl:param name="symbol"/>
    <xsl:choose>
      <xsl:when test="count($symbol/*) = 1">
        <xsl:call-template name="transformSymbolBit">
          <xsl:with-param name="bit" select="$symbol/*"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="transformSymbolBit">
          <xsl:with-param name="bit" select="$symbol/*[1]"/>
        </xsl:call-template>
        <xsl:for-each select="$symbol/*[position()>1]">
          <xsl:text>, </xsl:text>
          <xsl:call-template name="transformSymbolBit">
            <xsl:with-param name="bit" select="."/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="transformSymbolBit">
    <xsl:param name="bit"/>
    <xsl:choose>
      <xsl:when test="local-name($bit) = 'text'">
        <xsl:text>keyword("</xsl:text>
        <xsl:value-of select="$bit/text()"/>
        <xsl:text>")</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'choice'">
        <xsl:text>(</xsl:text>
        <xsl:for-each select="$bit/*">
          <xsl:call-template name="transformSymbolBit">
            <xsl:with-param name="bit" select="."/>
          </xsl:call-template>
          <xsl:text>; </xsl:text>
        </xsl:for-each>
        <xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'optional'">
        <xsl:text>(</xsl:text>
          <xsl:call-template name="transformSymbol">
            <xsl:with-param name="symbol" select="$bit"/>
          </xsl:call-template>
        <xsl:text>; epsilon)</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'newline'">
        <xsl:text>newline</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'less-indentation'">
        <xsl:text>lessindentation</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'same-indentation'">
        <xsl:text>sameindentation</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'more-indentation'">
        <xsl:text>moreindentation</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'eight-spaces'">
        <xsl:text>string("        ")</xsl:text>
      </xsl:when>
      <!-- not implemented: counter, layout, font, anything -->
    </xsl:choose>
  </xsl:template>

  <xsl:template name="symbol2definition">
    <xsl:param name="predicate"/>
    <xsl:param name="symbol"/>
    <xsl:value-of select="$predicate"/>
    <xsl:text> --> </xsl:text>
    <xsl:call-template name="transformSymbol">
      <xsl:with-param name="symbol" select="$symbol"/>
    </xsl:call-template>
    <xsl:text>.
</xsl:text>
  </xsl:template>
</xsl:stylesheet>
