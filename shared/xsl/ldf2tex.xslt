<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:bgf="http://planet-sl.org/bgf"
    xmlns:ldf="http://planet-sl.org/ldf"
    xmlns:ldx="http://planet-sl.org/ldx"
    xmlns:xhtml="http://www.w3.org/1999/xhtml">

  <xsl:import href="bgf2bnf.xslt" />

  <xsl:output
      method="text"
      encoding="UTF-8"
      omit-xml-declaration="yes"
      />

  <xsl:template match="/ldf:document">
    <xsl:text>
\documentclass{article}
\setcounter{secnumdepth}{2}
\setcounter{tocdepth}{2}
\begin{document}
    </xsl:text>
    <!-- title -->
    <xsl:text>\title{</xsl:text>
    <xsl:value-of select="titlePage/topic"/>
    <!-- version or edition -->
    <xsl:if test="titlePage/version">
      <xsl:text> v.</xsl:text>
      <xsl:value-of select="titlePage/version"/>
    </xsl:if>
    <xsl:if test="titlePage/edition">
      <xsl:text> </xsl:text>
      <xsl:value-of select="titlePage/edition"/>
      <xsl:text>ed</xsl:text>
    </xsl:if>
    <xsl:text>}</xsl:text>
    <!-- body/number or author -->
    <xsl:text>\author{</xsl:text>
    <xsl:choose>
      <xsl:when test="titlePage/body">
        <xsl:call-template name="uppercase">
          <xsl:with-param name="string" select="titlePage/body"/>
        </xsl:call-template>
        <xsl:value-of select="titlePage/number"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="titlePage/author"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>}</xsl:text>
    <!-- status -->
    <xsl:if test="titlePage/status != 'unknown'">
      <xsl:text>\institute{</xsl:text>
      <xsl:value-of select="titlePage/status"/>
      <xsl:text>}</xsl:text>
    </xsl:if>
    <!-- date -->
    <xsl:text>\date{</xsl:text>
    <xsl:value-of select="titlePage/date"/>
    <xsl:text>}</xsl:text>
    <!-- titlePage done -->
    <xsl:text>\maketitle\tableofcontents</xsl:text>
    <!-- placeholder: not implemented -->
    <!-- frontMatter -->
    <xsl:for-each select="frontMatter/*">
      <xsl:call-template name="sectionize">
        <xsl:with-param name="target" select="."/>
      </xsl:call-template>
      <xsl:call-template name="process-SimpleSection">
        <xsl:with-param name="section" select="."/>
      </xsl:call-template>
    </xsl:for-each>
    <!-- lists -->
    <xsl:for-each select="lists/*">
      <xsl:call-template name="process-ListOfTerms">
        <xsl:with-param name="list" select="."/>
      </xsl:call-template>
    </xsl:for-each>
    <!-- lexicalPart -->
    <xsl:for-each select="lexicalPart/*">
      <xsl:call-template name="sectionize">
        <xsl:with-param name="target" select="."/>
      </xsl:call-template>
      <xsl:call-template name="process-SimpleSection">
        <xsl:with-param name="section" select="."/>
      </xsl:call-template>
    </xsl:for-each>
    <xsl:apply-templates select="core"/>
    <xsl:text>\end{document}</xsl:text>
  </xsl:template>

  <xsl:template match="list">
    <xsl:text>\begin{itemize}</xsl:text>
    <xsl:for-each select="item">
      <xsl:text>\item </xsl:text>
      <xsl:value-of select="."/>
    </xsl:for-each>
    <xsl:text>\end{itemize}</xsl:text>
  </xsl:template>

  <xsl:template name="process-ListOfTerms">
    <xsl:param name="list"/>
    <xsl:text>\section{</xsl:text>
    <xsl:choose>
      <xsl:when test="$list/title">
        <xsl:value-of select="$list/title"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>List of </xsl:text>
        <xsl:value-of select="local-name($list)"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>}</xsl:text>
    <xsl:text>\begin{description}</xsl:text>
    <xsl:for-each select="$list/term">
      <xsl:text>\item[</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>] </xsl:text>
      <xsl:choose>
        <xsl:when test="count(definition/p) = 1 and count(definition/list) = 0">
          <xsl:value-of select="definition/p"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="process-text">
            <xsl:with-param name="text" select="definition"/>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    <xsl:text>\end{description}</xsl:text>
  </xsl:template>

  <xsl:template match="formula">
    <xsl:text>$$</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>$$</xsl:text>
  </xsl:template>

  <xsl:template name="process-text">
    <xsl:param name="text"/>
    <xsl:for-each select="$text/node()">
      <xsl:choose>
        <xsl:when test="local-name() = 'id'">
          <xsl:text>\label{</xsl:text>
          <xsl:value-of select="."/>
          <xsl:text>}</xsl:text>
        </xsl:when>
        <xsl:when test="local-name() = 'title'"/>
        <xsl:when test="local-name() = 'author'"/>
        <xsl:when test="local-name() = 'p'">
          <xsl:value-of select="."/>
        </xsl:when>
        <xsl:when test="local-name() = 'list'">
          <xsl:apply-templates select="."/>
        </xsl:when>
        <xsl:when test="local-name() = 'formula'">
          <xsl:apply-templates select="."/>
        </xsl:when>
        <xsl:when test="local-name() = 'production'">
          <xsl:text>
	        \begin{verbatim}</xsl:text>
          <xsl:apply-templates select="."/>
          <xsl:text>\end{verbatim}
	
	      </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="sectionize">
    <xsl:param name="target"/>
    <xsl:text>\section{</xsl:text>
    <xsl:choose>
      <xsl:when test="$target/title">
        <xsl:value-of select="$target/title"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <!-- can be replaced with a unified CamelCase2Whitespace -->
          <xsl:when test="local-name() = 'lineContinuations'">
            <xsl:text>Line continuations</xsl:text>
          </xsl:when>
          <xsl:when test="local-name() = 'designGoals'">
            <xsl:text>Design goals</xsl:text>
          </xsl:when>
          <xsl:when test="local-name() = 'normativeReferences'">
            <xsl:text>Normative references</xsl:text>
          </xsl:when>
          <xsl:when test="local-name() = 'documentStructure'">
            <xsl:text>Document structure</xsl:text>
          </xsl:when>
          <xsl:when test="local-name() = 'whatsnew'">
            <xsl:text>What's new</xsl:text>
          </xsl:when>
          <xsl:when test="local-name() = 'languageOverview'">
            <xsl:text>Language overview</xsl:text>
          </xsl:when>
          <!-- end of CamelCase2Whitespace -->
          <xsl:otherwise>
            <xsl:call-template name="capitalise">
              <xsl:with-param name="section" select="$target"/>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xsl:template name="subsectionize">
    <xsl:param name="target"/>
    <xsl:text>\subsection{</xsl:text>
    <xsl:choose>
      <xsl:when test="$target/title">
        <xsl:value-of select="$target/title"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="capitalise">
          <xsl:with-param name="section" select="$target"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xsl:template name="subsubsectionize">
    <xsl:param name="target"/>
    <xsl:text>\subsubsection{</xsl:text>
    <xsl:choose>
      <xsl:when test="$target/title">
        <xsl:value-of select="$target/title"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="capitalise">
          <xsl:with-param name="section" select="$target"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <xsl:template name="process-SimpleSection">
    <xsl:param name="section"/>
    <xsl:for-each select="$section/*">
      <xsl:choose>
        <xsl:when test="local-name() = 'id'">
          <xsl:text>\label{</xsl:text>
          <xsl:value-of select="."/>
          <xsl:text>}</xsl:text>
        </xsl:when>
        <xsl:when test="local-name() = 'title'"/>
        <xsl:when test="local-name() = 'author'"/>
        <xsl:when test="local-name() = 'production'">
          <xsl:text>
	        \begin{verbatim}</xsl:text>
          <xsl:apply-templates select="."/>
          <xsl:text>\end{verbatim}
	
	      </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="process-text">
            <xsl:with-param name="text" select="."/>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="process-StructuredSection">
    <xsl:param name="section"/>
      <xsl:for-each select="$section/*">
      <xsl:choose>
        <xsl:when test="local-name() = 'subtopic'">
          <xsl:call-template name="subsectionize">
            <xsl:with-param name="target" select="."/>
          </xsl:call-template>
          <xsl:call-template name="process-StructuredSection">
            <xsl:with-param name="section" select="."/>
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="local-name() = 'author'"/>
        <xsl:when test="local-name() = 'title'"/>
        <xsl:when test="local-name() = 'id'">
          <xsl:text>\label{</xsl:text>
          <xsl:value-of select="."/>
          <xsl:text>}</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:choose>
            <xsl:when test="local-name($section) = 'subtopic'">
              <xsl:call-template name="subsubsectionize">
            <xsl:with-param name="target" select="."/>
          </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="subsectionize">
                <xsl:with-param name="target" select="."/>
              </xsl:call-template>
            </xsl:otherwise>
          </xsl:choose>
           <xsl:call-template name="process-SimpleSection">
            <xsl:with-param name="section" select="."/>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="core">
    <xsl:call-template name="sectionize">
      <xsl:with-param name="target" select="."/>
    </xsl:call-template>
    <xsl:call-template name="process-StructuredSection">
      <xsl:with-param name="section" select="."/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="capitalise">
    <xsl:param name="section"/>
    <xsl:value-of select="concat(translate(substring(local-name($section),1,1), 'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ'), substring(local-name($section),2))"/>
  </xsl:template>

  <xsl:template name="uppercase">
    <xsl:param name="string"/>
    <xsl:value-of select="translate($string, 'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/>
  </xsl:template>

  <!--xsl:value-of select="."/-->
  <xsl:template match="text">
    <p xmlns="http://www.w3.org/1999/xhtml">
      <xsl:for-each select="node()">
        <xsl:choose>
          <xsl:when test="namespace-uri() = 'http://planet-sl.org/ldx'">
            <xsl:apply-templates select="."/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of select="."/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </p>
  </xsl:template>

</xsl:stylesheet>