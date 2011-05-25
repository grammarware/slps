<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="html" encoding="UTF-8" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"/>
	<xsl:param name="gname"/>
	<xsl:template match="/bgf:grammar">
		<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
			<head>
				<title>Browsable Grammar</title>
				<style type="text/css">
		          h1, table { text-align: center; }
		          .label, .sel { color: green; }
		          .marked { background-color: #FFE5B4;}
		          .nt { color: blue; font-weight: bold; }
		          .t { color: red;  font-style:italic; }
		          .meta { color: green; font-style:italic; font-family: Roman, "Times New Roman", serif; }
		          h6 { text-align: right; }
		          .date { font-size: small; }

		          .note
		          {
		          text-align: right;
		          font-weight: bold;
		          margin: 0px;
		          }
		          .frame, pre
		          {
		          border: 1px solid black;
		          border-spacing: 2px;
		          border-collapse: collapse;
		          background-color: #ECECEC;
		          }

		        </style>
			</head>
			<body>
				<xsl:for-each select="./bgf:*">
					<pre>
						<xsl:apply-templates select="."/>
					</pre>
				</xsl:for-each>
			</body>
		</html>
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
		<span xmlns="http://www.w3.org/1999/xhtml" class="meta">
			<xsl:text>+</xsl:text>
		</span>
	</xsl:template>
	<xsl:template match="star">
		<xsl:apply-templates select="./*"/>
		<span xmlns="http://www.w3.org/1999/xhtml" class="meta">
			<xsl:text>*</xsl:text>
		</span>
	</xsl:template>
	<xsl:template match="optional">
		<xsl:apply-templates select="./*"/>
		<span xmlns="http://www.w3.org/1999/xhtml" class="meta">
			<xsl:text>?</xsl:text>
		</span>
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
					<xsl:text>string</xsl:text>
				</xsl:when>
				<xsl:otherwise>
					<xsl:text>int</xsl:text>
				</xsl:otherwise>
			</xsl:choose>
		</span>
	</xsl:template>
	<xsl:template match="epsilon">
		<xsl:text>ε</xsl:text>
	</xsl:template>
	<xsl:template match="empty">
		<span xmlns="http://www.w3.org/1999/xhtml" class="meta">
			<xsl:text>empty</xsl:text>
		</span>
	</xsl:template>
	<xsl:template match="any">
		<span xmlns="http://www.w3.org/1999/xhtml" class="meta">
			<xsl:text>any</xsl:text>
		</span>
	</xsl:template>
	<xsl:template match="nonterminal">
		<xsl:choose>
			<xsl:when test=". = 'anyURI'">
				<span xmlns="http://www.w3.org/1999/xhtml" class="meta">
					<xsl:text>any-uri</xsl:text>
				</span>
			</xsl:when>
			<xsl:when test=". = 'ID'">
				<span xmlns="http://www.w3.org/1999/xhtml" class="meta">
					<xsl:text>id</xsl:text>
				</span>
			</xsl:when>
			<xsl:otherwise>
				<xsl:call-template name="linknt">
					<xsl:with-param name="nt" select="."/>
				</xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="selectable">
		<span xmlns="http://www.w3.org/1999/xhtml" class="sel">
			<xsl:value-of select="selector"/>
		</span>
		<xsl:text>::</xsl:text>
		<xsl:choose>
			<xsl:when test="local-name(bgf:expression/*) = 'star'                    or local-name(bgf:expression/*) = 'optional'                    or local-name(bgf:expression/*) = 'plus'">
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
		<xsl:for-each select="./bgf:expression[position()&gt;1]">
			<xsl:text> </xsl:text>
			<xsl:apply-templates select="./*"/>
		</xsl:for-each>
		<xsl:text>)</xsl:text>
	</xsl:template>
	<!-- inner choices - BNF bar -->
	<xsl:template match="choice">
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="./bgf:expression[1]/*"/>
		<xsl:for-each select="./bgf:expression[position()&gt;1]">
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
				<xsl:for-each select="$expr/sequence/bgf:expression[position()&gt;1]">
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
		<a xmlns="http://www.w3.org/1999/xhtml" class="nt" href="{$gname}#{$nt}">
			<xsl:value-of select="$nt"/>
		</a>
	</xsl:template>
	<xsl:template name="displaynt">
		<xsl:param name="nt"/>
		<a xmlns="http://www.w3.org/1999/xhtml" class="nt" name="{$nt}">
			<xsl:value-of select="$nt"/>
		</a>
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
		<a xmlns="http://www.w3.org/1999/xhtml" class="label" name="{$l}">
			<xsl:value-of select="$l"/>
		</a>
		<xsl:text>] </xsl:text>
	</xsl:template>
</xsl:stylesheet>
