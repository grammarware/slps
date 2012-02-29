<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:exbgf="http://planet-sl.org/exbgf" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:param name="bgf"/>
	<xsl:variable name="grammar" select="document($bgf)/bgf:grammar"/>
	<xsl:template match="/exbgf:sequence">
		<xbgf:sequence>
			<xsl:apply-templates select="*"/>
		</xbgf:sequence>
	</xsl:template>
	<xsl:template match="xbgf:*">
		<!--
			We continue to support all of XBGF.
		-->
		<xsl:copy-of select="."/>
	</xsl:template>
	<xsl:template match="exbgf:deyaccifyH">
		<!--
			Deyaccification that works on horizontal productions.
		-->
		<xbgf:vertical>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:vertical>
		<xbgf:deyaccify>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:deyaccify>
	</xsl:template>
	<xsl:template match="exbgf:deyaccifyC">
		<!--
			Deyaccification that works on horizontal productions with inner choices.
		-->
		<xbgf:distribute>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:distribute>
		<xbgf:vertical>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:vertical>
		<xbgf:deyaccify>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:deyaccify>
	</xsl:template>
	<xsl:template match="exbgf:massageO2C">
		<!--
			Massaging an optional nonterminal to a choice between the nonterminal and ε.
		-->
		<xbgf:massage>
			<bgf:expression>
				<optional>
					<bgf:expression>
						<xsl:copy-of select="*"/>
					</bgf:expression>
				</optional>
			</bgf:expression>
			<bgf:expression>
				<choice>
					<bgf:expression>
						<xsl:copy-of select="*"/>
					</bgf:expression>
					<bgf:expression>
						<epsilon/>
					</bgf:expression>
				</choice>
			</bgf:expression>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:massageDouble">
		<!--
			Massaging an double regular (plus, star, optional).
		-->
		<xbgf:massage>
			<bgf:expression>
				<xsl:element name="{local-name(*[1])}">
					<bgf:expression>
						<xsl:element name="{local-name(*[1])}">
							<bgf:expression>
								<xsl:copy-of select="*[2]"/>
							</bgf:expression>
						</xsl:element>
					</bgf:expression>
				</xsl:element>
			</bgf:expression>
			<bgf:expression>
				<xsl:element name="{local-name(*[1])}">
					<bgf:expression>
						<xsl:copy-of select="*[2]"/>
					</bgf:expression>
				</xsl:element>
			</bgf:expression>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:massageOP2S">
		<!--
			Massaging an optional plus of to a star.
		-->
		<xbgf:massage>
			<bgf:expression>
				<optional>
					<bgf:expression>
						<plus>
							<bgf:expression>
								<xsl:copy-of select="*"/>
							</bgf:expression>
						</plus>
					</bgf:expression>
				</optional>
			</bgf:expression>
			<bgf:expression>
				<star>
					<bgf:expression>
						<xsl:copy-of select="*"/>
					</bgf:expression>
				</star>
			</bgf:expression>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:inlineP2S">
		<!--
			Inlining a plus to become a star (instead of optional plus).
			Does not work as intended, because it needs an immediate input grammar.
		-->
		<xsl:variable name="nt" select="."/>
		<xbgf:inline>
			<xsl:value-of select="$nt"/>
		</xbgf:inline>
		<xbgf:massage>
			<bgf:expression>
				<optional>
					<xsl:copy-of select="$grammar/bgf:production[nonterminal=$nt]/bgf:expression"/>
				</optional>
			</bgf:expression>
			<bgf:expression>
				<star>
					<xsl:copy-of select="$grammar/bgf:production[nonterminal=$nt]/bgf:expression/plus/bgf:expression"/>
				</star>
			</bgf:expression>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:uniteMany">
		<!--
			Making one fresh nonterminal out of definitions of multiple nonterminals.
		-->
		<xbgf:rename>
			<nonterminal>
				<from>
					<xsl:value-of select="add[1]"/>
				</from>
				<to>
					<xsl:value-of select="yield"/>
				</to>
			</nonterminal>
		</xbgf:rename>
		<xsl:for-each select="add[position()&gt;1]">
			<xbgf:unite>
				<add>
					<xsl:value-of select="."/>
				</add>
				<to>
					<xsl:value-of select="../yield"/>
				</to>
			</xbgf:unite>
		</xsl:for-each>
	</xsl:template>
</xsl:stylesheet>
