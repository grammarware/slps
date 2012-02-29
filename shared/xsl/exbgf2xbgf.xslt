<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:exbgf="http://planet-sl.org/exbgf" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
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
		<xsl:message>[EXBGF] deyaccifyH</xsl:message>
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
		<xsl:message>[EXBGF] deyaccifyC</xsl:message>
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
		<xsl:message>[EXBGF] massageO2C</xsl:message>
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
		<xsl:message>[EXBGF] massageDouble</xsl:message>
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
		<xsl:message>[EXBGF] massageOP2S</xsl:message>
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
	<xsl:template match="exbgf:promoteP2S">
		<!--
			Promoting a nonterminal that is defined as a plus, to a full-fledged star.
			(It needs to be used as an optional in order for this to work)
			equivalent to xbgf:inline + exbgf:massageOP2S + xbgf:extract
		-->
		<xsl:message>[EXBGF] promoteP2S</xsl:message>
		<xbgf:inline>
			<xsl:value-of select="nonterminal"/>
		</xbgf:inline>
		<xbgf:massage>
			<bgf:expression>
				<optional>
					<bgf:expression>
						<plus>
							<bgf:expression>
								<nonterminal>
									<xsl:value-of select="starof"/>
								</nonterminal>
							</bgf:expression>
						</plus>
					</bgf:expression>
				</optional>
			</bgf:expression>
			<bgf:expression>
				<star>
					<bgf:expression>
						<nonterminal>
							<xsl:value-of select="starof"/>
						</nonterminal>
					</bgf:expression>
				</star>
			</bgf:expression>
		</xbgf:massage>
		<xbgf:extract>
			<bgf:production>
				<nonterminal>
					<xsl:choose>
						<xsl:when test="newname">
							<xsl:value-of select="newname"/>
						</xsl:when>
						<xsl:otherwise>
							<xsl:value-of select="nonterminal"/>
						</xsl:otherwise>
					</xsl:choose>
				</nonterminal>
				<bgf:expression>
					<star>
						<bgf:expression>
							<nonterminal>
								<xsl:value-of select="starof"/>
							</nonterminal>
						</bgf:expression>
					</star>
				</bgf:expression>
			</bgf:production>
		</xbgf:extract>
	</xsl:template>
	<xsl:template match="exbgf:promoteY2S">
		<!--
			Promoting a nonterminal that is defined as a yaccified plus, to a full-fledged star.
			(It needs to be used as an optional in order for this to work)
			equivalent to exbgf:inlineY + exbgf:massageOP2S + xbgf:extract
			equivalent to xbgf:deyaccify + exbgf:promoteP2S
		-->
		<xsl:message>[EXBGF] promoteY2S</xsl:message>
		<xbgf:deyaccify>
			<nonterminal>
				<xsl:value-of select="nonterminal"/>
			</nonterminal>
		</xbgf:deyaccify>
		<xbgf:inline>
			<xsl:value-of select="nonterminal"/>
		</xbgf:inline>
		<xbgf:massage>
			<bgf:expression>
				<optional>
					<bgf:expression>
						<plus>
							<bgf:expression>
								<nonterminal>
									<xsl:value-of select="starof"/>
								</nonterminal>
							</bgf:expression>
						</plus>
					</bgf:expression>
				</optional>
			</bgf:expression>
			<bgf:expression>
				<star>
					<bgf:expression>
						<nonterminal>
							<xsl:value-of select="starof"/>
						</nonterminal>
					</bgf:expression>
				</star>
			</bgf:expression>
		</xbgf:massage>
		<xbgf:extract>
			<bgf:production>
				<nonterminal>
					<xsl:choose>
						<xsl:when test="newname">
							<xsl:value-of select="newname"/>
						</xsl:when>
						<xsl:otherwise>
							<xsl:value-of select="nonterminal"/>
						</xsl:otherwise>
					</xsl:choose>
				</nonterminal>
				<bgf:expression>
					<star>
						<bgf:expression>
							<nonterminal>
								<xsl:value-of select="starof"/>
							</nonterminal>
						</bgf:expression>
					</star>
				</bgf:expression>
			</bgf:production>
		</xbgf:extract>
	</xsl:template>
	<!--
		Inlining a plus to become a star (instead of optional plus).
		Does not work as intended, because it needs an immediate input grammar.
	-->
	<!-- <xsl:template match="exbgf:inlineP2S">
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
		</xsl:template> -->
	<xsl:template match="exbgf:uniteMany">
		<!--
			Making one fresh nonterminal out of definitions of multiple nonterminals.
		-->
		<xsl:message>[EXBGF] uniteMany</xsl:message>
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
	<xsl:template match="exbgf:inlineY">
		<!--
			Properly inline a nonterminal defined with yaccified production rules.
		-->
		<xsl:message>[EXBGF] inlineY</xsl:message>
		<xbgf:deyaccify>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:deyaccify>
		<xbgf:inline>
			<xsl:value-of select="."/>
		</xbgf:inline>
	</xsl:template>
	<xsl:template match="exbgf:inlineV">
		<!--
			Properly inline a nonterminal defined vertically (by multiple production rules).
		-->
		<xsl:message>[EXBGF] inlineV</xsl:message>
		<xbgf:horizontal>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:horizontal>
		<xbgf:inline>
			<xsl:value-of select="."/>
		</xbgf:inline>
	</xsl:template>
</xsl:stylesheet>
