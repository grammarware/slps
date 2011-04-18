<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:links="http://planet-sl.org/links" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="html" encoding="UTF-8" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"/>
	<xsl:template match="/links:repository">
		<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
			<head>
				<title>
					<xsl:value-of select="@name"/>
				</title>
				<style type="text/css">h1{text-align:center}</style>
				<link href="slps.css" rel="stylesheet" type="text/css"/>
			</head>
			<body style="background-color:#9C9;">
				<h1><xsl:value-of select="@name"/></h1>
				<xsl:for-each select="list">
					<h2>
						<xsl:value-of select="title"/>
					</h2>
					<ul>
						<xsl:apply-templates select="item"/>
					</ul>
					<hr/>
				</xsl:for-each>
			</body>
		</html>
	</xsl:template>
	<xsl:template match="item">
		<li xmlns="http://www.w3.org/1999/xhtml">
			<strong>
				<a>
					<xsl:attribute name="href">
						<xsl:value-of select="link"/>
					</xsl:attribute>
					<xsl:value-of select="title"/>
				</a>
			</strong>
			<xsl:text> (</xsl:text>
			<xsl:value-of select="author[1]"/>
			<xsl:for-each select="author[position()&gt;1]">
				<xsl:text>, </xsl:text>
				<xsl:value-of select="."/>
			</xsl:for-each>
			<xsl:text>) — </xsl:text>
			<xsl:value-of select="version"/>
			<xsl:text>, </xsl:text>
			<xsl:value-of select="form"/>
			<p>
				<xsl:value-of select="comment"/>
			</p>
		</li>
	</xsl:template>
</xsl:stylesheet>
