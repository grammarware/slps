<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="text" encoding="UTF-8"/>
	<xsl:template match="/zoo">
		<xsl:text>all:
</xsl:text>
		<xsl:for-each select="language">
			<xsl:for-each select="version/grammar">
				<xsl:variable name="filename">
					<xsl:choose>
						<xsl:when test="../../short">
							<xsl:value-of select="handle"/>
						</xsl:when>
						<xsl:when test="substring-after(handle,concat(translate(../../name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ +#“”','abcdefghijklmnopqrstuvwxyz_ps__'),'/'))=''">
							<xsl:value-of select="concat(translate(../../name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ +#“”','abcdefghijklmnopqrstuvwxyz_ps__'),concat('/',translate(handle,'/','-')))"/>
						</xsl:when>
						<xsl:otherwise>
							<xsl:value-of select="concat(translate(../../name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ +#“”','abcdefghijklmnopqrstuvwxyz_ps__'),concat('/',substring-after(handle,'/')))"/>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:variable>
				<xsl:text>	cp ../topics/grammars/</xsl:text>
				<xsl:value-of select="handle"/>
				<xsl:text>/grammar.bgf </xsl:text>
				<xsl:value-of select="/zoo/name"/>
				<xsl:text>/</xsl:text>
				<xsl:value-of select="$filename"/>
				<xsl:text>.bgf
	_dev/format </xsl:text>
				<xsl:value-of select="/zoo/name"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="$filename"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="handle"/>
				<xsl:text>
</xsl:text>
			</xsl:for-each>
			<xsl:for-each select="version/grammarset/grammarname">
				<!-- clone! -->
				<xsl:variable name="filename">
					<xsl:choose>
						<xsl:when test="../../short">
							<xsl:value-of select="concat(translate(../../short,'ABCDEFGHIJKLMNOPQRSTUVWXYZ +#“”','abcdefghijklmnopqrstuvwxyz_ps__'),concat('/',translate(.,'ABCDEFGHIJKLMNOPQRSTUVWXYZ +#“”','abcdefghijklmnopqrstuvwxyz_ps__')))"/>
						</xsl:when>
						<xsl:when test="../../name">
							<xsl:value-of select="concat(translate(../../name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ +#“”','abcdefghijklmnopqrstuvwxyz_ps__'),concat('/',translate(.,'ABCDEFGHIJKLMNOPQRSTUVWXYZ +#“”','abcdefghijklmnopqrstuvwxyz_ps__')))"/>
						</xsl:when>
						<xsl:otherwise>
							<xsl:text>error</xsl:text>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:variable>
				<xsl:text>	cp ../topics/grammars/</xsl:text>
				<xsl:value-of select="."/>
				<xsl:text>/grammar.bgf </xsl:text>
				<xsl:value-of select="/zoo/name"/>
				<xsl:text>/</xsl:text>
				<xsl:value-of select="$filename"/>
				<xsl:text>.bgf
	_dev/format </xsl:text>
				<xsl:value-of select="/zoo/name"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="$filename"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="."/>
				<xsl:text>
			</xsl:text>
			</xsl:for-each>
		</xsl:for-each>
	</xsl:template>
</xsl:stylesheet>
