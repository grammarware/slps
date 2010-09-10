<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/spec">
		<bgf:grammar xmlns:bgf="http://planet-sl.org/bgf">
			<xsl:for-each select="//scrap">
				<xsl:comment>
					<xsl:text>Scrap </xsl:text>
					<xsl:value-of select="head"/>
				</xsl:comment>
				<xsl:for-each select=".//prod">
					<bgf:production>
						<xsl:if test="@id">
							<label>
								<xsl:value-of select="@id"/>
							</label>
						</xsl:if>
						<nonterminal>
							<xsl:value-of select="lhs"/>
						</nonterminal>
						<xsl:choose>
							<xsl:when test="count(rhs)=1">
								<xsl:apply-templates select="rhs"/>
							</xsl:when>
							<xsl:otherwise>
								<bgf:expression>
									<choice>
										<xsl:apply-templates select="rhs"/>
									</choice>
								</bgf:expression>
							</xsl:otherwise>
						</xsl:choose>
					</bgf:production>
				</xsl:for-each>
			</xsl:for-each>
		</bgf:grammar>
	</xsl:template>
	<xsl:template match="rhs">
		<bgf:expression xmlns:bgf="http://planet-sl.org/bgf">
			<xsl:for-each select="node()">
				<xsl:choose>
<!-- regular nonterminal -->
					<xsl:when test="local-name(.) = 'nt'">
						<nonterminal>
							<xsl:value-of select="."/>
						</nonterminal>
					</xsl:when>
<!-- externally defined nonterminal -->
					<xsl:when test="local-name(.) = 'xnt'">
						<nonterminal>
							<xsl:value-of select="."/>
						</nonterminal>
					</xsl:when>
					<xsl:when test=". = '?'">
						<postfix-optional/>
					</xsl:when>
					<xsl:when test=". = '*'">
						<postfix-star/>
					</xsl:when>
					<xsl:when test="substring(.,1,1) = '|'">
						<xsl:call-template name="map-multiple-symbols">
							<xsl:with-param name="list" select="substring(.,2)"/>
						</xsl:call-template>
					</xsl:when>
					<xsl:otherwise>
						<xsl:call-template name="map-multiple-symbols">
							<xsl:with-param name="list" select="."/>
						</xsl:call-template>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:for-each>
		</bgf:expression>
	</xsl:template>
	<xsl:template name="map-one-symbol">
		<xsl:param name="node"/>
		<xsl:choose>
			<xsl:when test="$node = '?'">
				<postfix-optional/>
			</xsl:when>
<!-- terminal -->
			<xsl:when test="substring($node,1,1) = &quot;'&quot;">
				<terminal>
					<xsl:value-of select="substring-before(substring($node,2),&quot;'&quot;)"/>
				</terminal>
				<xsl:call-template name="map-multiple-symbols">
					<xsl:with-param name="list" select="substring-after(substring($node,2),&quot;'&quot;)"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:when test="substring($node,1,1) = ' '">
				<leading-space>
					<xsl:value-of select="substring($node,2)"/>
				</leading-space>
				<xsl:call-template name="map-multiple-symbols">
					<xsl:with-param name="list" select="substring($node,2)"/>
				</xsl:call-template>
			</xsl:when>
<!-- empty string -->
			<xsl:when test="$node = ''"/>
			<xsl:otherwise>
				<unknown>
					<xsl:copy-of select="$node"/>
				</unknown>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template name="map-multiple-symbols">
		<xsl:param name="list"/>
		<xsl:variable name="newlist" select="concat(normalize-space($list), ' ')"/>
		<xsl:variable name="first" select="substring-before($newlist, ' ')"/>
		<xsl:variable name="remaining" select="substring-after($newlist, ' ')"/>
		<xsl:call-template name="map-one-symbol">
			<xsl:with-param name="node" select="$first"/>
		</xsl:call-template>
		<xsl:if test="$remaining">
			<xsl:call-template name="map-multiple-symbols">
				<xsl:with-param name="list" select="$remaining"/>
			</xsl:call-template>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet>
