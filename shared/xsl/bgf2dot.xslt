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
   <xsl:text>digraph generated{</xsl:text>
   <xsl:for-each select="./bgf:production">
     <xsl:call-template name="findDependencies">
       <xsl:with-param name="start" select="./nonterminal"/>
       <xsl:with-param name="traverse" select="./bgf:expression"/>
     </xsl:call-template>
   </xsl:for-each>
   <xsl:text>}</xsl:text>
 </xsl:template>

  <xsl:template name="findDependencies">
    <xsl:param name="start"/>
    <xsl:param name="traverse"/>
    <xsl:for-each select="$traverse//nonterminal">
      <xsl:if test=".!=$start">
        <xsl:value-of select="$start"/>
        <xsl:text>-></xsl:text>
        <xsl:value-of select="."/>
        <xsl:text>;</xsl:text>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

 </xsl:stylesheet>
