<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:cbgf="http://planet-sl.org/cbgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:param name="context"/>
	<xsl:variable name="cntxt" select="document($context)/bgf:grammar"/>
	<xsl:template match="/cbgf:relationship">
		<cbgf:relationship>
			<xsl:copy-of select="*[position() &lt;= count(/cbgf:relationship/*[todo][1]/preceding-sibling::*)]"/>
			<xsl:apply-templates select="*[position() = 1+count(/cbgf:relationship/*[todo][1]/preceding-sibling::*)]"/>
			<xsl:copy-of select="*[position() &gt; 1+count(/cbgf:relationship/*[todo][1]/preceding-sibling::*)]"/>
		</cbgf:relationship>
	</xsl:template>
	<xsl:template match="cbgf:deyaccify-yaccify">
		<xsl:variable name="n" select="todo/nonterminal"/>
		<cbgf:deyaccify-yaccify>
			<xsl:copy-of select="$cntxt/bgf:production[nonterminal=$n]"/>
		</cbgf:deyaccify-yaccify>
	</xsl:template>
	<xsl:template match="cbgf:factor-factor">
		<cbgf:factor-factor>
			<!-- TODO -->
			<xsl:copy-of select="*"/>
		</cbgf:factor-factor>
	</xsl:template>
	<xsl:template match="cbgf:eliminate-introduce">
		<xsl:variable name="n" select="todo/nonterminal"/>
		<cbgf:eliminate-introduce>
			<xsl:copy-of select="$cntxt/bgf:production[nonterminal=$n]"/>
		</cbgf:eliminate-introduce>
	</xsl:template>
	<xsl:template match="cbgf:inline-extract">
		<xsl:variable name="n" select="todo"/>
		<cbgf:inline-extract>
			<xsl:copy-of select="$cntxt/bgf:production[nonterminal=$n]"/>
		</cbgf:inline-extract>
	</xsl:template>
	<xsl:template match="cbgf:permute-permute">
		<xsl:variable name="n" select="todo/bgf:production/nonterminal"/>
		<xsl:variable name="l" select="todo/bgf:production/label"/>
		<cbgf:permute-permute>
			<xsl:choose>
				<xsl:when test="$l">
					<xsl:copy-of select="$cntxt/bgf:production[nonterminal=$n and label=$l]"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:copy-of select="$cntxt/bgf:production[nonterminal=$n]"/>
				</xsl:otherwise>
			</xsl:choose>
			<xsl:copy-of select="todo/*"/>
		</cbgf:permute-permute>
	</xsl:template>
	<xsl:template match="cbgf:undefine-define">
		<xsl:variable name="n" select="todo/nonterminal"/>
		<cbgf:undefine-define>
			<xsl:copy-of select="$cntxt/bgf:production[nonterminal=$n]"/>
		</cbgf:undefine-define>
	</xsl:template>
	<xsl:template match="cbgf:reroot-reroot">
		<cbgf:reroot-reroot>
			<from>
				<xsl:copy-of select="$cntxt/root"/>
			</from>
			<to>
				<xsl:copy-of select="todo/root"/>
			</to>
		</cbgf:reroot-reroot>
	</xsl:template>
	<xsl:template match="cbgf:unlabel-designate">
		<xsl:variable name="l" select="todo/label"/>
		<cbgf:unlabel-designate>
			<xsl:copy-of select="$cntxt/bgf:production[label=$l]"/>
		</cbgf:unlabel-designate>
	</xsl:template>
	<xsl:template match="cbgf:equate-clone">
		<xsl:variable name="n1" select="todo/align"/>
		<xsl:variable name="n2" select="todo/with"/>
		<cbgf:equate-clone>
			<align>
				<xsl:copy-of select="$cntxt/bgf:production[nonterminal=$n1]"/>
			</align>
			<with>
				<xsl:value-of select="$n2"/>
			</with>
			<in>
				<xsl:for-each select="$cntxt/bgf:production[*//nonterminal=$n1]">
					<xsl:choose>
						<xsl:when test="label">
							<label>
								<xsl:value-of select="label"/>
							</label>
						</xsl:when>
						<xsl:otherwise>
							<nonterminal>
								<xsl:value-of select="nonterminal"/>
							</nonterminal>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:for-each>
			</in>
		</cbgf:equate-clone>
	</xsl:template>
	<xsl:template match="cbgf:unite-split">
		<xsl:variable name="n1" select="todo/add"/>
		<xsl:variable name="n2" select="todo/to"/>
		<xsl:choose>
			<xsl:when test="$cntxt/bgf:production[nonterminal=$n1]">
				<cbgf:unite-split>
					<add>
						<xsl:copy-of select="$cntxt/bgf:production[nonterminal=$n1]"/>
					</add>
					<to>
						<xsl:copy-of select="$cntxt/bgf:production[nonterminal=$n2]"/>
					</to>
					<in>
						<xsl:for-each select="$cntxt/bgf:production[*//nonterminal=$n1]">
							<xsl:choose>
								<xsl:when test="label">
									<label>
										<xsl:value-of select="label"/>
									</label>
								</xsl:when>
								<xsl:otherwise>
									<nonterminal>
										<xsl:value-of select="nonterminal"/>
									</nonterminal>
								</xsl:otherwise>
							</xsl:choose>
						</xsl:for-each>
					</in>
				</cbgf:unite-split>
			</xsl:when>
			<xsl:otherwise>
				<!-- merging two undefined nonterminals -->
				<cbgf:replace-replace>
					<bgf:expression>
						<nonterminal>
							<xsl:value-of select="$n1"/>
						</nonterminal>
					</bgf:expression>
					<bgf:expression>
						<nonterminal>
							<xsl:value-of select="$n2"/>
						</nonterminal>
					</bgf:expression>
					<in>
						<xsl:for-each select="$cntxt/bgf:production[*//nonterminal=$n1]">
							<xsl:choose>
								<xsl:when test="label">
									<label>
										<xsl:value-of select="label"/>
									</label>
								</xsl:when>
								<xsl:otherwise>
									<nonterminal>
										<xsl:value-of select="nonterminal"/>
									</nonterminal>
								</xsl:otherwise>
							</xsl:choose>
						</xsl:for-each>
					</in>
				</cbgf:replace-replace>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
