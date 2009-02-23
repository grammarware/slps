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
            <xsl:with-param name="content" select="definition"/>
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
    <xsl:param name="content"/>
    <xsl:for-each select="$content/node()">
      <xsl:choose>
        <xsl:when test="local-name() = 'p'">
          <!-- essentially a copy-of, but with a namespace-->
          <p xmlns="http://www.w3.org/1999/xhtml">
            <xsl:value-of select="."/>
          </p>
        </xsl:when>
        <xsl:when test="local-name() = 'list'">
          <ul xmlns="http://www.w3.org/1999/xhtml">
            <xsl:for-each select="item">
              <li>
                <xsl:value-of select="."/>
              </li>
            </xsl:for-each>
          </ul>
        </xsl:when>
        <xsl:when test="local-name() = 'formula'">
          <center xmlns="http://www.w3.org/1999/xhtml">
            <em>
              <xsl:value-of select="."/>
            </em>
          </center>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="process-SimpleSection">
    <xsl:param name="section"/>
    <xsl:if test="$section/id">
      <xsl:text>\label{</xsl:text>
      <xsl:attribute name = "name">
        <xsl:value-of select="$section/id"/>
      </xsl:attribute>
      <xsl:text>}</xsl:text>
    </xsl:if>
    <xsl:text>\section{</xsl:text>
    <xsl:choose>
      <xsl:when test="$section/title">
        <xsl:value-of select="$section/title"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="capitalise">
          <xsl:with-param name="section" select="$section"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>}</xsl:text>
    <xsl:call-template name="process-text">
      <xsl:with-param name="content" select="$section/content"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="core">
    <xsl:text>\section{</xsl:text>
    <xsl:choose>
      <xsl:when test="title">
        <xsl:value-of select="title"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="capitalise">
          <xsl:with-param name="section" select="."/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>}</xsl:text>
    <xsl:for-each select="*">
      <xsl:choose>
        <xsl:when test="local-name() = 'id'">
          <xsl:text>\label{</xsl:text>
          <xsl:value-of select="."/>
          <xsl:text>}</xsl:text>
        </xsl:when>
        <xsl:when test="local-name() = 'title'"></xsl:when>
        <xsl:when test="local-name() = 'author'"></xsl:when>
        <xsl:when test="local-name() = 'production'">
          <xsl:text>
	        \begin{verbatim}
	      </xsl:text>
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

  <xsl:template match="subtopic">
    <xsl:text>\subsection{</xsl:text>
    <xsl:choose>
      <xsl:when test="title">
        <xsl:value-of select="title"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="capitalise">
          <xsl:with-param name="section" select="."/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>}</xsl:text>
    <xsl:for-each select="*">
      <xsl:choose>
        <xsl:when test="local-name() = 'id'">
          <xsl:text>\label{</xsl:text>
            <xsl:value-of select="."/>
          <xsl:text>}</xsl:text>
        </xsl:when>
        <xsl:when test="local-name() = 'title'"></xsl:when>
        <xsl:when test="local-name() = 'author'"></xsl:when>
        <xsl:when test="local-name() = 'production'">
          <xsl:text>\begin{verbatim}</xsl:text>
          <xsl:apply-templates select="."/>
          <xsl:text>\end{verbatim}</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
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

  <xsl:template match="grammar">
    <blockquote xmlns="http://www.w3.org/1999/xhtml" class="frame">
      <p class="note">
        <small>
          <a>
            <xsl:attribute name = "href">
              <xsl:value-of select="./@language"/>
            </xsl:attribute>
            Grammar productions
            <xsl:if test="./@version">
              (ver. <xsl:value-of select="./@version"/>)
            </xsl:if>
          </a>
        </small>
      </p>
      <xsl:apply-templates select="./bgf:*"/>
    </blockquote>
  </xsl:template>

  <xsl:template match="sample">
    <xsl:call-template name="treatsamples">
      <xsl:with-param name="s" select="."/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="runnable">
    <xsl:call-template name="treatrunnables">
      <xsl:with-param name="e" select="."/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="getname">
    <xsl:param name="title"/>
    <xsl:choose>
      <xsl:when test="$title/author">
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="uppercase">
          <xsl:with-param name="string" select="$title/body"/>
        </xsl:call-template>
        <xsl:value-of select="$title/number"/>
        <xsl:text>: </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="$title/topic"/>
    <xsl:text> </xsl:text>
    <xsl:choose>
      <xsl:when test="$title/edition">
        <xsl:value-of select="$title/edition"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$title/version"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="$title/status != 'unknown'">
    <xsl:text> (</xsl:text>
    <xsl:value-of select="$title/status"/>
    <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template name="treatsamples">
    <xsl:param name="s"/>
    <xsl:if test="$s/@id">
      <a xmlns="http://www.w3.org/1999/xhtml">
        <xsl:attribute name = "name">
          <xsl:value-of select="$s/@id"/>
        </xsl:attribute>
      </a>
    </xsl:if>
    <blockquote xmlns="http://www.w3.org/1999/xhtml" class="frame">
      <p class="note">
        <small>
          <a>
            <xsl:attribute name = "href">
              <xsl:value-of select="$s/@language"/>
            </xsl:attribute>
            Source code sample
            <xsl:if test="$s/@version">
              (ver. <xsl:value-of select="$s/@version"/>)
            </xsl:if>
          </a>
        </small>
      </p>
      <br/>
      <pre>
        <xsl:value-of select="$s"/>
      </pre>
    </blockquote>
  </xsl:template>

  <xsl:template name="treatrunnables">
    <xsl:param name="e"/>
    <blockquote xmlns="http://www.w3.org/1999/xhtml" class="frame">
      <p class="note">
        <small>
          <a>
            <xsl:attribute name = "href">
              <xsl:value-of select="$e/@language"/>
            </xsl:attribute>
            Sample execution
            <xsl:if test="$e/@version">
              (ver. <xsl:value-of select="$e/@version"/>)
            </xsl:if>
          </a>
        </small>
      </p>
      <br/>
      <code>
        <strong>
          <a>
            <xsl:attribute name = "href">#<xsl:value-of select="$e/context"/>
            </xsl:attribute>
            <xsl:value-of select="$e/main"/>
          </a>
        </strong>
        <xsl:apply-templates select="$e/argument"/>
      </code>
      <br/>
      <p class="note">
        <small>
          (Should return <xsl:value-of select="$e/yields"/>)
        </small>
      </p>
    </blockquote>
  </xsl:template>

  <xsl:template match="argument" xml:space="preserve">
    <strong xmlns="http://www.w3.org/1999/xhtml">
      <xsl:value-of select="."/>
    </strong>
  </xsl:template>

  <xsl:template match="section">
    <h3 xmlns="http://www.w3.org/1999/xhtml">
      <xsl:value-of select="./title"/>
    </h3>
    <xsl:apply-templates select="./content"/>
  </xsl:template>

  <!-- END OF LDF, START OF BGF -->
  
  <xsl:template name="car">
    <xsl:param name="expr"/>
    <xsl:apply-templates select="$expr/*"/>
  </xsl:template>

  <xsl:template name="cdr">
    <xsl:param name="expr"/>
    <br xmlns="http://www.w3.org/1999/xhtml"/>
    | <xsl:apply-templates select="$expr/*"/>
  </xsl:template>
  
  <!-- END OF BGF, START OF LDX -->
  
  <xsl:template match="ldx:reference">
    <code xmlns="http://www.w3.org/1999/xhtml">
      <a>
        <xsl:attribute name = "href">
          #<xsl:value-of select="."/>
        </xsl:attribute>
        <xsl:value-of select="."/>
      </a>
    </code>
  </xsl:template>


</xsl:stylesheet>
