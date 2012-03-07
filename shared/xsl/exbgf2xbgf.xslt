<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:exbgf="http://planet-sl.org/exbgf" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/exbgf:sequence">
		<xbgf:sequence>
			<xsl:apply-templates select="*"/>
		</xbgf:sequence>
	</xsl:template>
	<xsl:template match="exbgf:atomic">
		<!-- 
			An atomic transformation sequence inherited from XBGF.

			TODO: kill it in XBGF
		-->
		<xsl:message>[EXBGF] atomic ::= atomic</xsl:message>
		<xbgf:atomic>
			<xsl:apply-templates select="*"/>
		</xbgf:atomic>
	</xsl:template>
	<xsl:template match="exbgf:shielded">
		<!-- 
			An atomic transformation sequence, shielding an entity with a temporarily introduced nonterminal.
		-->
		<xsl:message>[EXBGF] shielded ::= extract + ... + inline</xsl:message>
		<xbgf:extract>
			<bgf:production>
				<nonterminal>SHIELDED-ENTITY</nonterminal>
				<bgf:expression>
					<xsl:copy-of select="entity/*"/>
				</bgf:expression>
			</bgf:production>
			<xsl:if test="context">
				<in>
					<nonterminal>
						<xsl:value-of select="context"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:extract>
		<xsl:apply-templates select="*[local-name()!='entity' and local-name()!='context']"/>
		<xbgf:inline>SHIELDED-ENTITY</xbgf:inline>
	</xsl:template>
	<xsl:template match="exbgf:shieldedD">
		<!-- 
			An atomic transformation sequence, shielding an entity with a temporarily introduced nonterminal that disappears during transformation.
		-->
		<xsl:message>[EXBGF] shieldedD ::= extract + ... + eliminate</xsl:message>
		<xbgf:extract>
			<bgf:production>
				<nonterminal>SHIELDED-ENTITY</nonterminal>
				<bgf:expression>
					<xsl:copy-of select="entity/*"/>
				</bgf:expression>
			</bgf:production>
		</xbgf:extract>
		<xsl:apply-templates select="*[local-name()!='entity']"/>
		<xbgf:eliminate>
			<nonterminal>SHIELDED-ENTITY</nonterminal>
		</xbgf:eliminate>
	</xsl:template>
	<xsl:template match="exbgf:shieldedU">
		<!-- 
			An atomic transformation sequence, shielding an entity that does not exist by the beginning of transformation.
		-->
		<xsl:message>[EXBGF] shieldedU ::= introduce + ... + inline</xsl:message>
		<xbgf:introduce>
			<bgf:production>
				<nonterminal>SHIELDED-ENTITY</nonterminal>
				<bgf:expression>
					<xsl:copy-of select="entity/*"/>
				</bgf:expression>
			</bgf:production>
		</xbgf:introduce>
		<xsl:apply-templates select="*[local-name()!='entity']"/>
		<xbgf:inline>SHIELDED-ENTITY</xbgf:inline>
	</xsl:template>
	<xsl:template match="exbgf:tempunfold">
		<!-- 
			An atomic transformation sequence, temporarily unfolding a nonterminal.
		-->
		<xsl:message>[EXBGF] tempunfold ::= unfold + ... + fold</xsl:message>
		<xbgf:unfold>
			<nonterminal>
				<xsl:value-of select="unfolded"/>
			</nonterminal>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:unfold>
		<xsl:apply-templates select="*[local-name()!='unfolded' and local-name()!='in']"/>
		<xbgf:fold>
			<nonterminal>
				<xsl:value-of select="unfolded"/>
			</nonterminal>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:fold>
	</xsl:template>
	<xsl:template match="exbgf:atomicI">
		<!-- 
			A boosted atomic transformation sequence which transparently looks at the context
			(horizontally defined nonterminal with inner choices) as a vertical one.
			atomic ( exbgf:distributeV + ... + xbgf:vertical )
			atomic ( xbgf:horizontal + xbgf:distribute + ... + xbgf:vertical )
		-->
		<xsl:message>[EXBGF] atomicI ::= atomic ( horizontal + distribute + ... + vertical )</xsl:message>
		<xbgf:horizontal>
			<nonterminal>
				<xsl:value-of select="context"/>
			</nonterminal>
		</xbgf:horizontal>
		<xbgf:distribute>
			<nonterminal>
				<xsl:value-of select="context"/>
			</nonterminal>
		</xbgf:distribute>
		<xsl:apply-templates select="*[local-name()!='context']"/>
		<xbgf:vertical>
			<nonterminal>
				<xsl:value-of select="context"/>
			</nonterminal>
		</xbgf:vertical>
	</xsl:template>
	<xsl:template match="exbgf:atomicC">
		<!-- 
			A boosted atomic transformation sequence which transparently looks at the context
			(horizontally defined nonterminal with inner choices) as a vertical one.
			atomic ( exbgf:distributeH + ... + xbgf:horizontal )
			atomic ( xbgf:distribute + xbgf:vertical + ... + xbgf:horizontal )
		-->
		<xsl:message>[EXBGF] atomicC ::= atomic ( distribute + vertical + ... + horizontal )</xsl:message>
		<xbgf:distribute>
			<nonterminal>
				<xsl:value-of select="context"/>
			</nonterminal>
		</xbgf:distribute>
		<xbgf:vertical>
			<nonterminal>
				<xsl:value-of select="context"/>
			</nonterminal>
		</xbgf:vertical>
		<xsl:apply-templates select="*[local-name()!='context']"/>
		<xbgf:horizontal>
			<nonterminal>
				<xsl:value-of select="context"/>
			</nonterminal>
		</xbgf:horizontal>
	</xsl:template>
	<xsl:template match="exbgf:atomicH">
		<!-- 
			A boosted atomic transformation sequence which transparently looks at a horizontally defined nonterminal (context) as a vertical one.
			
			atomic ( xbgf:vertical + ... + xbgf:horizontal )
		-->
		<xsl:message>[EXBGF] atomicH ::= atomic ( vertical + ... + horizontal )</xsl:message>
		<xbgf:vertical>
			<nonterminal>
				<xsl:value-of select="context"/>
			</nonterminal>
		</xbgf:vertical>
		<xsl:apply-templates select="*[local-name()!='context']"/>
		<xbgf:horizontal>
			<nonterminal>
				<xsl:value-of select="context"/>
			</nonterminal>
		</xbgf:horizontal>
	</xsl:template>
	<xsl:template match="exbgf:atomicV">
		<!-- 
			A boosted atomic transformation sequence which transparently looks at a vertically defined nonterminal (context) as a horizontal one.

			atomic ( xbgf:horizontal + ... + xbgf:vertical)
		-->
		<xsl:message>[EXBGF] atomicV ::= atomic ( horizontal + ... + vertical )</xsl:message>
		<xbgf:horizontal>
			<nonterminal>
				<xsl:value-of select="context"/>
			</nonterminal>
		</xbgf:horizontal>
		<xsl:apply-templates select="*[local-name()!='context']"/>
		<xbgf:vertical>
			<nonterminal>
				<xsl:value-of select="context"/>
			</nonterminal>
		</xbgf:vertical>
	</xsl:template>
	<xsl:template match="xbgf:*">
		<!--
			We continue to support all of XBGF.
		-->
		<xsl:copy-of select="."/>
	</xsl:template>
	<xsl:template match="exbgf:yaccifyH">
		<!--
			Yaccification that produces horizontal productions.
		-->
		<xsl:message>[EXBGF] yaccifyH ::= yaccify + horizontal</xsl:message>
		<xbgf:yaccify>
			<xsl:copy-of select="bgf:production"/>
		</xbgf:yaccify>
		<xbgf:horizontal>
			<xsl:copy-of select="bgf:production[1]/nonterminal"/>
		</xbgf:horizontal>
	</xsl:template>
	<xsl:template match="exbgf:deyaccifyH">
		<!--
			Deyaccification that works on horizontal productions.
		-->
		<xsl:message>[EXBGF] deyaccifyH ::= vertical + deyaccify</xsl:message>
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
			
			exbgf:distributeH + xbgf:deyaccify
		-->
		<xsl:message>[EXBGF] deyaccifyC ::= distribute + vertical + deyaccify</xsl:message>
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
	<xsl:template match="exbgf:regroupLR">
		<!--
			Massaging (A B)* A to A (B A)*
			
			inverse of exbgf:regroupRL
		-->
		<xsl:message>[EXBGF] regroupLR ::= massage</xsl:message>
		<xbgf:massage>
			<bgf:expression>
				<sequence>
					<bgf:expression>
						<star>
							<bgf:expression>
								<sequence>
									<xsl:copy-of select="*[1]"/>
									<xsl:copy-of select="*[2]"/>
								</sequence>
							</bgf:expression>
						</star>
					</bgf:expression>
					<xsl:copy-of select="*[1]"/>
				</sequence>
			</bgf:expression>
			<bgf:expression>
				<sequence>
					<xsl:copy-of select="*[1]"/>
					<bgf:expression>
						<star>
							<bgf:expression>
								<sequence>
									<xsl:copy-of select="*[2]"/>
									<xsl:copy-of select="*[1]"/>
								</sequence>
							</bgf:expression>
						</star>
					</bgf:expression>
				</sequence>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:regroupRL">
		<!--
			Massaging A (B A)* to (A B)* A
			
			inverse of exbgf:regroupLR
		-->
		<xsl:message>[EXBGF] regroupRL ::= massage</xsl:message>
		<xbgf:massage>
			<bgf:expression>
				<sequence>
					<xsl:copy-of select="*[1]"/>
					<bgf:expression>
						<star>
							<bgf:expression>
								<sequence>
									<xsl:copy-of select="*[2]"/>
									<xsl:copy-of select="*[1]"/>
								</sequence>
							</bgf:expression>
						</star>
					</bgf:expression>
				</sequence>
			</bgf:expression>
			<bgf:expression>
				<sequence>
					<bgf:expression>
						<star>
							<bgf:expression>
								<sequence>
									<xsl:copy-of select="*[1]"/>
									<xsl:copy-of select="*[2]"/>
								</sequence>
							</bgf:expression>
						</star>
					</bgf:expression>
					<xsl:copy-of select="*[1]"/>
				</sequence>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:massage1S2P">
		<!--
			Massaging E E* to E+
		-->
		<xsl:message>[EXBGF] massage1S2P ::= massage</xsl:message>
		<xbgf:massage>
			<bgf:expression>
				<sequence>
					<bgf:expression>
						<xsl:copy-of select="*[1]"/>
					</bgf:expression>
					<bgf:expression>
						<star>
							<bgf:expression>
								<xsl:copy-of select="*[1]"/>
							</bgf:expression>
						</star>
					</bgf:expression>
				</sequence>
			</bgf:expression>
			<bgf:expression>
				<plus>
					<bgf:expression>
						<xsl:copy-of select="*[1]"/>
					</bgf:expression>
				</plus>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:massageS12P">
		<!--
			Massaging E* E to E+
		-->
		<xsl:message>[EXBGF] massageS12P ::= massage</xsl:message>
		<xbgf:massage>
			<bgf:expression>
				<sequence>
					<bgf:expression>
						<star>
							<bgf:expression>
								<xsl:copy-of select="*[1]"/>
							</bgf:expression>
						</star>
					</bgf:expression>
					<bgf:expression>
						<xsl:copy-of select="*[1]"/>
					</bgf:expression>
				</sequence>
			</bgf:expression>
			<bgf:expression>
				<plus>
					<bgf:expression>
						<xsl:copy-of select="*[1]"/>
					</bgf:expression>
				</plus>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:massageC2S">
		<!--
			Massaging E+ | ε to E*
		-->
		<xsl:message>[EXBGF] massageC2S ::= massage</xsl:message>
		<xbgf:massage>
			<bgf:expression>
				<choice>
					<bgf:expression>
						<plus>
							<bgf:expression>
								<xsl:copy-of select="*[1]"/>
							</bgf:expression>
						</plus>
					</bgf:expression>
					<bgf:expression>
						<epsilon/>
					</bgf:expression>
				</choice>
			</bgf:expression>
			<bgf:expression>
				<star>
					<bgf:expression>
						<xsl:copy-of select="*[1]"/>
					</bgf:expression>
				</star>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:massage12C">
		<!--
			Massaging E to S1::E | S2::E
		-->
		<xsl:message>[EXBGF] massage12C ::= massage</xsl:message>
		<xbgf:massage>
			<bgf:expression>
				<xsl:copy-of select="*[1]"/>
			</bgf:expression>
			<bgf:expression>
				<choice>
					<xsl:for-each select="selector">
						<bgf:expression>
							<selectable>
								<xsl:copy-of select="."/>
								<bgf:expression>
									<xsl:copy-of select="../*[1]"/>
								</bgf:expression>
							</selectable>
						</bgf:expression>
					</xsl:for-each>
				</choice>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:massageO2C">
		<!--
			Massaging an optional expression to a choice between the expression and ε.
			inverse of: exbgf:massageC2O
		-->
		<xsl:message>[EXBGF] massageO2C ::= massage</xsl:message>
		<xbgf:massage>
			<bgf:expression>
				<optional>
					<bgf:expression>
						<xsl:copy-of select="*[1]"/>
					</bgf:expression>
				</optional>
			</bgf:expression>
			<bgf:expression>
				<choice>
					<bgf:expression>
						<xsl:copy-of select="*[1]"/>
					</bgf:expression>
					<bgf:expression>
						<epsilon/>
					</bgf:expression>
				</choice>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:massageC2O">
		<!--
			Massaging a choice between an expression and ε to the optional expression.
			inverse of: exbgf:massageO2C
		-->
		<xsl:message>[EXBGF] massageC2O ::= massage</xsl:message>
		<xbgf:massage>
			<bgf:expression>
				<choice>
					<bgf:expression>
						<xsl:copy-of select="*[1]"/>
					</bgf:expression>
					<bgf:expression>
						<epsilon/>
					</bgf:expression>
				</choice>
			</bgf:expression>
			<bgf:expression>
				<optional>
					<bgf:expression>
						<xsl:copy-of select="*[1]"/>
					</bgf:expression>
				</optional>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:massage-double">
		<!--
			Massaging an double regular (plus, star, optional).
		-->
		<xsl:message>[EXBGF] massage-double ::= massage</xsl:message>
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
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:massageOP2S">
		<!--
			Massaging an optional plus of to a star.
		-->
		<xsl:message>[EXBGF] massageOP2S ::= massage</xsl:message>
		<xbgf:massage>
			<bgf:expression>
				<optional>
					<bgf:expression>
						<plus>
							<bgf:expression>
								<xsl:copy-of select="*[1]"/>
							</bgf:expression>
						</plus>
					</bgf:expression>
				</optional>
			</bgf:expression>
			<bgf:expression>
				<star>
					<bgf:expression>
						<xsl:copy-of select="*[1]"/>
					</bgf:expression>
				</star>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:promoteP2S">
		<!--
			Promoting a nonterminal that is defined as a plus, to a full-fledged star.
			(It needs to be used as an optional in order for this to work)
			equivalent to xbgf:inline + exbgf:massageOP2S + xbgf:extract
		-->
		<xsl:message>[EXBGF] promoteP2S ::= inline + massage + extract</xsl:message>
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
			equivalent to exbgf:inlineYO + xbgf:extract
		-->
		<xsl:message>[EXBGF] promoteY2S ::= deyaccify + inline + massage + extract</xsl:message>
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
	<xsl:template match="exbgf:double-inline">
		<!--
			Inlining a nonterminal that is defined with a chain production rule.
			
			Can be implemented as inline + inline, but we go beyond that.
		-->
		<xsl:message>[EXBGF] double-inline ::= unchain + inline</xsl:message>
		<xbgf:unchain>
			<bgf:production>
				<nonterminal><xsl:value-of select="."/></nonterminal>
				<bgf:expression>
					<nonterminal><xsl:value-of select="@chainto"/></nonterminal>
				</bgf:expression>
			</bgf:production>
		</xbgf:unchain>
		<xbgf:inline>
			<xsl:value-of select="."/>
		</xbgf:inline>

	</xsl:template>
	<xsl:template match="exbgf:inlineYO">
		<!--
			Inlining a nonterminal that is defined as a yaccified plus, in an optional context.
			(It will be masages into a star)
			equivalent to exbgf:inlineY + exbgf:massageOP2S
			equivalent to xbgf:deyaccify + xbgf:inline + xbgf:massage
		-->
		<xsl:message>[EXBGF] inlineYO ::= deyaccify + inline + massage</xsl:message>
		<xbgf:deyaccify>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:deyaccify>
		<xbgf:inline>
			<xsl:value-of select="."/>
		</xbgf:inline>
		<xbgf:massage>
			<bgf:expression>
				<optional>
					<bgf:expression>
						<plus>
							<bgf:expression>
								<nonterminal>
									<xsl:value-of select="@starof"/>
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
							<xsl:value-of select="@starof"/>
						</nonterminal>
					</bgf:expression>
				</star>
			</bgf:expression>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="exbgf:inlineYF">
		<!--
			Inlining a yaccified nonterminal, with additional folding in between deyaccification and inlining.
			equivalent to xbgf:deyaccify + xbgf:fold + xbgf:inline
		-->
		<xsl:message>[EXBGF] inlineYF ::= deyaccify + fold + inline</xsl:message>
		<xbgf:deyaccify>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:deyaccify>
		<xbgf:fold>
			<nonterminal>
				<xsl:value-of select="@fold"/>
			</nonterminal>
			<in>
				<nonterminal>
					<xsl:value-of select="."/>
				</nonterminal>
			</in>
		</xbgf:fold>
		<xbgf:inline>
			<xsl:value-of select="."/>
		</xbgf:inline>
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
	<xsl:template match="exbgf:unite-many">
		<!--
			Making one fresh nonterminal out of definitions of multiple nonterminals.
		-->
		<xsl:choose>
			<xsl:when test="add[1]!=''">
				<xsl:message>[EXBGF] unite-many ::= rename + unite...</xsl:message>
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
			</xsl:when>
			<xsl:otherwise>
				<xsl:message>[EXBGF] unite-many ::= unite...</xsl:message>
			</xsl:otherwise>
		</xsl:choose>
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
	<xsl:template match="exbgf:eliminate-many">
		<!--
			Eliminating multiple nonterminals.
		-->
		<xsl:message>[EXBGF] eliminate-many ::= eliminate...</xsl:message>
		<xsl:for-each select="nonterminal">
			<xbgf:eliminate>
				<xsl:copy-of select="."/>
			</xbgf:eliminate>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="exbgf:distributeV">
		<!--
			Distribute over vertically defined production rules.
			horizontal + distribute
		-->
		<xsl:message>[EXBGF] distributeV ::= horizontal + distribute</xsl:message>
		<xbgf:horizontal>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:horizontal>
		<xbgf:distribute>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:distribute>
	</xsl:template>
	<xsl:template match="exbgf:distributeH">
		<!--
			Distribute and produce vertically defined production rules.
			distribute + vertical
		-->
		<xsl:message>[EXBGF] distributeH ::= distribute + vertical</xsl:message>
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
	</xsl:template>
	<xsl:template match="exbgf:inlineY">
		<!--
			Properly inline a nonterminal defined with yaccified production rules.
		-->
		<xsl:message>[EXBGF] inlineY ::= deyaccify + inline</xsl:message>
		<xbgf:deyaccify>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:deyaccify>
		<xbgf:inline>
			<xsl:value-of select="."/>
		</xbgf:inline>
	</xsl:template>
	<xsl:template match="exbgf:inlineYH">
		<!--
			Properly inline a nonterminal defined with horizontal yaccified production rules.
			
			= exbgf:deyaccifyH + xbgf:inline
			= xbgf:vertical + exbgf:inlineY
		-->
		<xsl:message>[EXBGF] inlineYH ::= vertical + deyaccify + inline</xsl:message>
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
		<xbgf:inline>
			<xsl:value-of select="."/>
		</xbgf:inline>
	</xsl:template>
	<xsl:template match="exbgf:inlineV">
		<!--
			Properly inline a nonterminal defined vertically (by multiple production rules).
		-->
		<xsl:message>[EXBGF] inlineV ::= horizontal + inline</xsl:message>
		<xbgf:horizontal>
			<nonterminal>
				<xsl:value-of select="."/>
			</nonterminal>
		</xbgf:horizontal>
		<xbgf:inline>
			<xsl:value-of select="."/>
		</xbgf:inline>
	</xsl:template>
	<xsl:template match="exbgf:inlineU">
		<!--
			Properly inline an undefined nonterminal.
		-->
		<xsl:message>[EXBGF] inlineU ::= define + inline</xsl:message>
		<xbgf:define>
			<xsl:copy-of select="bgf:production"/>
		</xbgf:define>
		<xbgf:inline>
			<xsl:value-of select="bgf:production/nonterminal"/>
		</xbgf:inline>
	</xsl:template>
	<xsl:template match="exbgf:chainV">
		<!--
			Properly chain a nonterminal defined vertically (by multiple production rules).
			
			= atomicV ( chain )
		-->
		<xsl:message>[EXBGF] chainV ::= horizontal + chain + vertical</xsl:message>
		<xbgf:horizontal>
			<nonterminal>
				<xsl:value-of select="from"/>
			</nonterminal>
		</xbgf:horizontal>
		<xbgf:chain>
			<bgf:production>
				<nonterminal>
					<xsl:value-of select="from"/>
				</nonterminal>
				<bgf:expression>
					<nonterminal>
						<xsl:value-of select="to"/>
					</nonterminal>
				</bgf:expression>
			</bgf:production>
		</xbgf:chain>
		<xbgf:vertical>
			<nonterminal>
				<xsl:value-of select="to"/>
			</nonterminal>
		</xbgf:vertical>
	</xsl:template>
	<xsl:template match="exbgf:chainX">
		<!--
			Chains a nonterminal and adds another vertical production to it.
			
			In other words, goes from A: XYZ to A: B; B: XYZ; A: CD
			Conceptually, an injection in the nonterminal call graph.
			
			= chain + addV
		-->
		<xsl:message>[EXBGF] chainX ::= chain + add</xsl:message>
		<xbgf:chain>
			<bgf:production>
				<nonterminal>
					<xsl:value-of select="from"/>
				</nonterminal>
				<bgf:expression>
					<nonterminal>
						<xsl:value-of select="to[1]"/>
					</nonterminal>
				</bgf:expression>
			</bgf:production>
		</xbgf:chain>
		<xsl:for-each select="to[position()&gt;1]">
			<xbgf:add>
				<vertical>
					<bgf:production>
						<nonterminal>
							<xsl:value-of select="../from"/>
						</nonterminal>
						<bgf:expression>
							<nonterminal>
								<xsl:value-of select="."/>
							</nonterminal>
						</bgf:expression>
					</bgf:production>
				</vertical>
			</xbgf:add>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="exbgf:introduceH">
		<!--
			Introduce a nonterminal horizontally for simplicity, wanting to see vertical production rules in the grammar.
		-->
		<xsl:message>[EXBGF] introduceH ::= introduce + vertical</xsl:message>
		<xbgf:introduce>
			<xsl:copy-of select="bgf:production"/>
		</xbgf:introduce>
		<xbgf:vertical>
			<xsl:copy-of select="bgf:production/nonterminal"/>
		</xbgf:vertical>
	</xsl:template>
	<xsl:template match="exbgf:defineH">
		<!--
			Define a nonterminal horizontally for simplicity, wanting to see vertical production rules in the grammar.
		-->
		<xsl:message>[EXBGF] defineH ::= define + vertical</xsl:message>
		<xbgf:define>
			<xsl:copy-of select="bgf:production"/>
		</xbgf:define>
		<xbgf:vertical>
			<xsl:copy-of select="bgf:production/nonterminal"/>
		</xbgf:vertical>
	</xsl:template>
	<xsl:template match="exbgf:reyaccify">
		<!--
			Yaccifies an already yaccified nonterminal differently.
		-->
		<xsl:message>[EXBGF] reyaccify ::= deyaccify + yaccify</xsl:message>
		<xbgf:deyaccify>
			<xsl:copy-of select="bgf:production[1]/nonterminal"/>
		</xbgf:deyaccify>
		<xbgf:yaccify>
			<xsl:copy-of select="bgf:production"/>
		</xbgf:yaccify>
	</xsl:template>
	<xsl:template match="exbgf:reextract">
		<!--
			Inlines a nonterminal and then extracts it again.
			(A much cleaner, folding-based, redefine)
		-->
		<xsl:message>[EXBGF] reextract ::= inline + extract</xsl:message>
		<xbgf:inline>
			<xsl:value-of select="bgf:production/nonterminal"/>
		</xbgf:inline>
		<xbgf:extract>
			<xsl:copy-of select="bgf:production"/>
		</xbgf:extract>
	</xsl:template>
	<xsl:template match="exbgf:reextractY">
		<!--
			Inlines a yaccified nonterminal and then extracts it again.
			(A much cleaner, folding-based, redefine)
		-->
		<xsl:message>[EXBGF] reextractY ::= deyaccify + inline + extract</xsl:message>
		<xbgf:deyaccify>
			<xsl:copy-of select="bgf:production/nonterminal"/>
		</xbgf:deyaccify>
		<xbgf:inline>
			<xsl:value-of select="bgf:production/nonterminal"/>
		</xbgf:inline>
		<xbgf:extract>
			<xsl:copy-of select="bgf:production"/>
		</xbgf:extract>
	</xsl:template>
	<xsl:template match="exbgf:extract-twice">
		<!--
			Extracts a nonterminal in two forms: first as given (xbgf:extract), then after a refactoring (xbgf:fold).
		-->
		<xsl:message>[EXBGF] extract-twice ::= extract + ... + fold</xsl:message>
		<xbgf:extract>
			<xsl:copy-of select="bgf:production"/>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in[1]"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:extract>
		<xsl:apply-templates select="*[local-name()!='production' and local-name()!='in']"/>
		<xsl:choose>
			<xsl:when test="in">
				<xsl:for-each select="in[position()&gt;1]">
					<xbgf:fold>
						<xsl:copy-of select="../bgf:production/nonterminal"/>
						<in>
							<nonterminal>
								<xsl:value-of select="."/>
							</nonterminal>
						</in>
					</xbgf:fold>
				</xsl:for-each>
			</xsl:when>
			<xsl:otherwise>
				<xbgf:fold>
					<xsl:copy-of select="bgf:production/nonterminal"/>
				</xbgf:fold>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="exbgf:redefine">
		<!--
			Undefines a nonterminal and then defines it again.
			It was a mistake to put this nonorthogonal feature into XBGF.
		-->
		<xsl:message>[EXBGF] redefine ::= undefine + define</xsl:message>
		<xbgf:undefine>
			<xsl:copy-of select="bgf:production/nonterminal"/>
		</xbgf:undefine>
		<xbgf:define>
			<xsl:copy-of select="bgf:production"/>
		</xbgf:define>
	</xsl:template>
	<xsl:template match="exbgf:redefineH">
		<!--
			Undefines a nonterminal and then defines it again, horizontally for simplicity.
			basically: xbgf:undefine + exbgf:defineH
			or: exbgf:redefine + xbgf:vertical
		-->
		<xsl:message>[EXBGF] redefineH ::= undefine + define + vertical</xsl:message>
		<xbgf:undefine>
			<xsl:copy-of select="bgf:production/nonterminal"/>
		</xbgf:undefine>
		<xbgf:define>
			<xsl:copy-of select="bgf:production"/>
		</xbgf:define>
		<xbgf:vertical>
			<xsl:copy-of select="bgf:production/nonterminal"/>
		</xbgf:vertical>
	</xsl:template>
	<xsl:template match="exbgf:metachangeT2N">
		<!--
			Turns a terminal into a nonterminal.
		-->
		<xsl:message>[EXBGF] metachangeT2N ::= replace</xsl:message>
		<xbgf:replace>
			<bgf:expression>
				<terminal>
					<xsl:value-of select="."/>
				</terminal>
			</bgf:expression>
			<bgf:expression>
				<nonterminal>
					<xsl:value-of select="."/>
				</nonterminal>
			</bgf:expression>
		</xbgf:replace>
	</xsl:template>
	<xsl:template match="exbgf:renameN">
		<!--
			Shorter notation for renaming a nonterminal.
			Scoped renamings do not exist in XBGF, so a replace is used instead.
		-->
		<xsl:choose>
			<xsl:when test="in">
				<xsl:message>[EXBGF] renameN ::= replace</xsl:message>
				<xbgf:replace>
					<bgf:expression>
						<nonterminal>
							<xsl:value-of select="from"/>
						</nonterminal>
					</bgf:expression>
					<bgf:expression>
						<nonterminal>
							<xsl:value-of select="to"/>
						</nonterminal>
					</bgf:expression>
					<in>
						<nonterminal>
							<xsl:value-of select="in"/>
						</nonterminal>
					</in>
				</xbgf:replace>
			</xsl:when>
			<xsl:otherwise>
				<xsl:message>[EXBGF] renameN ::= rename</xsl:message>
				<xbgf:rename>
					<nonterminal>
						<xsl:copy-of select="from"/>
						<xsl:copy-of select="to"/>
					</nonterminal>
				</xbgf:rename>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="exbgf:addV">
		<!--
			Shorter notation for adding a production rule vertically.
		-->
		<xsl:message>[EXBGF] addV ::= add</xsl:message>
		<xsl:for-each select="bgf:production">
			<xbgf:add>
				<vertical>
					<xsl:copy-of select="."/>
				</vertical>
			</xbgf:add>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="exbgf:addH">
		<!--
			Shorter notation for adding a production rule horizontally.
		-->
		<xsl:message>[EXBGF] addH ::= add</xsl:message>
		<xbgf:add>
			<horizontal>
				<xsl:copy-of select="bgf:production"/>
			</horizontal>
		</xbgf:add>
	</xsl:template>
	<xsl:template match="exbgf:addX">
		<!--
			Adds a production rule vertically to a previously horizontal definition.
			
			= vertical + addV
		-->
		<xsl:message>[EXBGF] addX ::= vertical + add</xsl:message>
		<xbgf:vertical>
			<xsl:copy-of select="bgf:production[1]/nonterminal"/>
		</xbgf:vertical>
		<xsl:for-each select="bgf:production">
			<xbgf:add>
				<vertical>
					<xsl:copy-of select="."/>
				</vertical>
			</xbgf:add>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="exbgf:removeV">
		<!--
			Shorter notation for removing a production rule vertically.
		-->
		<xsl:message>[EXBGF] removeV ::= remove</xsl:message>
		<xsl:for-each select="bgf:production">
			<xbgf:remove>
				<vertical>
					<xsl:copy-of select="."/>
				</vertical>
			</xbgf:remove>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="exbgf:removeH">
		<!--
			Shorter notation for removing a production rule horizontally.
		-->
		<xsl:message>[EXBGF] removeH ::= remove</xsl:message>
		<xbgf:remove>
			<horizontal>
				<xsl:copy-of select="bgf:production"/>
			</horizontal>
		</xbgf:remove>
	</xsl:template>
	<xsl:template match="exbgf:extractV">
		<!--
			Extracts a new production which context spans over several vertically defined production rules.
		-->
		<xsl:message>[EXBGF] extractV ::= horizontal + extract + vertical</xsl:message>
		<xsl:for-each select="context">
			<xbgf:horizontal>
				<nonterminal>
					<xsl:value-of select="."/>
				</nonterminal>
			</xbgf:horizontal>
		</xsl:for-each>
		<xbgf:extract>
			<xsl:copy-of select="bgf:production"/>
			<xsl:copy-of select="in"/>
		</xbgf:extract>
		<xsl:for-each select="context">
			<xbgf:vertical>
				<nonterminal>
					<xsl:value-of select="."/>
				</nonterminal>
			</xbgf:vertical>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="exbgf:extractC">
		<!--
			Extracts a new production which is a top-level choice that we want to see vertically.
		-->
		<xsl:message>[EXBGF] extractC ::= extract + vertical</xsl:message>
		<xbgf:extract>
			<xsl:copy-of select="bgf:production"/>
			<xsl:copy-of select="in"/>
		</xbgf:extract>
		<xbgf:vertical>
			<xsl:copy-of select="bgf:production/nonterminal"/>
		</xbgf:vertical>
	</xsl:template>
	<xsl:template match="exbgf:narrowS21">
		<!--
			Narrows an expression from E* to E.
		-->
		<xsl:message>[EXBGF] narrowS21 ::= narrow</xsl:message>
		<xbgf:narrow>
			<bgf:expression>
				<star>
					<bgf:expression>
						<xsl:copy-of select="*[local-name()!='in']"/>
					</bgf:expression>
				</star>
			</bgf:expression>
			<bgf:expression>
				<xsl:copy-of select="*[local-name()!='in']"/>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:narrow>
	</xsl:template>
	<xsl:template match="exbgf:narrowP21">
		<!--
			Narrows an expression from E+ to E.
		-->
		<xsl:message>[EXBGF] narrowP21 ::= narrow</xsl:message>
		<xbgf:narrow>
			<bgf:expression>
				<plus>
					<bgf:expression>
						<xsl:copy-of select="*[local-name()!='in']"/>
					</bgf:expression>
				</plus>
			</bgf:expression>
			<bgf:expression>
				<xsl:copy-of select="*[local-name()!='in']"/>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:narrow>
	</xsl:template>
	<xsl:template match="exbgf:narrowO21">
		<!--
			Narrows an expression from E? to E.
		-->
		<xsl:message>[EXBGF] narrowO21 ::= narrow</xsl:message>
		<xbgf:narrow>
			<bgf:expression>
				<optional>
					<bgf:expression>
						<xsl:copy-of select="*[local-name()!='in']"/>
					</bgf:expression>
				</optional>
			</bgf:expression>
			<bgf:expression>
				<xsl:copy-of select="*[local-name()!='in']"/>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:narrow>
	</xsl:template>
	<xsl:template match="exbgf:widen12O">
		<!--
			Widens an expression from E to E?.
		-->
		<xsl:message>[EXBGF] widen12O ::= widen</xsl:message>
		<xbgf:widen>
			<bgf:expression>
				<xsl:copy-of select="*[local-name()!='in']"/>
			</bgf:expression>
			<bgf:expression>
				<optional>
					<bgf:expression>
						<xsl:copy-of select="*[local-name()!='in']"/>
					</bgf:expression>
				</optional>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:widen>
	</xsl:template>
	<xsl:template match="exbgf:widen12S">
		<!--
			Widens an expression from E to E*.
		-->
		<xsl:message>[EXBGF] widen12S ::= widen</xsl:message>
		<xbgf:widen>
			<bgf:expression>
				<xsl:copy-of select="*[local-name()!='in']"/>
			</bgf:expression>
			<bgf:expression>
				<star>
					<bgf:expression>
						<xsl:copy-of select="*[local-name()!='in']"/>
					</bgf:expression>
				</star>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:widen>
	</xsl:template>
	<xsl:template match="exbgf:widen12P">
		<!--
			Widens an expression from E to E+.
		-->
		<xsl:message>[EXBGF] widen12P ::= widen</xsl:message>
		<xbgf:widen>
			<bgf:expression>
				<xsl:copy-of select="*[local-name()!='in']"/>
			</bgf:expression>
			<bgf:expression>
				<plus>
					<bgf:expression>
						<xsl:copy-of select="*[local-name()!='in']"/>
					</bgf:expression>
				</plus>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:widen>
	</xsl:template>
	<xsl:template match="exbgf:widenP2S">
		<!--
			Widens an expression from E+ to E*.
		-->
		<xsl:message>[EXBGF] widenP2S ::= widen</xsl:message>
		<xbgf:widen>
			<bgf:expression>
				<plus>
					<bgf:expression>
						<xsl:copy-of select="*[local-name()!='in']"/>
					</bgf:expression>
				</plus>
			</bgf:expression>
			<bgf:expression>
				<star>
					<bgf:expression>
						<xsl:copy-of select="*[local-name()!='in']"/>
					</bgf:expression>
				</star>
			</bgf:expression>
			<xsl:if test="in">
				<in>
					<nonterminal>
						<xsl:value-of select="in"/>
					</nonterminal>
				</in>
			</xsl:if>
		</xbgf:widen>
	</xsl:template>
	<xsl:template match="exbgf:unfoldV">
		<!--
			Unfolds (possibly in scope) a vertically defined nonterminal as a horizontal one.
		-->
		<xsl:message>[EXBGF] unfoldV ::= horizontal + unfold + vertical</xsl:message>
		<xbgf:horizontal>
			<xsl:copy-of select="nonterminal"/>
		</xbgf:horizontal>
		<xsl:if test="not(in)">
			<xbgf:unfold>
				<xsl:copy-of select="nonterminal"/>
			</xbgf:unfold>
		</xsl:if>
		<xsl:for-each select="in">
			<xbgf:unfold>
				<xsl:copy-of select="../nonterminal"/>
				<in>
					<nonterminal>
						<xsl:value-of select="."/>
					</nonterminal>
				</in>
			</xbgf:unfold>
		</xsl:for-each>
		<xbgf:vertical>
			<xsl:copy-of select="nonterminal"/>
		</xbgf:vertical>
	</xsl:template>
	<xsl:template match="exbgf:unfold">
		<!--
			Unfolds a nonterminal in multiple scopes.
		-->
		<xsl:message>[EXBGF] unfold ::= unfold...</xsl:message>
		<xsl:for-each select="nonterminal">
			<xsl:call-template name="unfold-in-context">
				<xsl:with-param name="nt" select="."/>
				<xsl:with-param name="context" select="../in"/>
			</xsl:call-template>
		</xsl:for-each>
	</xsl:template>
	<xsl:template name="unfold-in-context">
		<xsl:param name="nt"/>
		<xsl:param name="context"/>
		<!-- Unfolds a nonterminal in multiple scopes. -->
		<xsl:for-each select="$context">
			<xbgf:unfold>
				<xsl:copy-of select="$nt"/>
				<in>
					<nonterminal>
						<xsl:value-of select="."/>
					</nonterminal>
				</in>
			</xbgf:unfold>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="exbgf:unfoldY">
		<!--
			Unfolds (possibly in scope) a yaccified nonterminal.
		-->
		<xsl:message>[EXBGF] unfoldY ::= deyaccify + unfold</xsl:message>
		<xbgf:deyaccify>
			<xsl:copy-of select="nonterminal"/>
		</xbgf:deyaccify>
		<xsl:if test="not(in)">
			<xbgf:unfold>
				<xsl:copy-of select="nonterminal"/>
			</xbgf:unfold>
		</xsl:if>
		<xsl:for-each select="in">
			<xbgf:unfold>
				<xsl:copy-of select="../nonterminal"/>
				<in>
					<nonterminal>
						<xsl:value-of select="."/>
					</nonterminal>
				</in>
			</xbgf:unfold>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="exbgf:foldY">
		<!--
			Folds (possibly in scope) a yaccified nonterminal.
		-->
		<xsl:message>[EXBGF] foldY ::= deyaccify + fold</xsl:message>
		<xbgf:deyaccify>
			<xsl:copy-of select="nonterminal"/>
		</xbgf:deyaccify>
		<xsl:if test="not(in)">
			<xbgf:fold>
				<xsl:copy-of select="nonterminal"/>
			</xbgf:fold>
		</xsl:if>
		<xsl:for-each select="in">
			<xbgf:fold>
				<xsl:copy-of select="../nonterminal"/>
				<in>
					<nonterminal>
						<xsl:value-of select="."/>
					</nonterminal>
				</in>
			</xbgf:fold>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="exbgf:factor-out">
		<!--
			Factor an expression in a concise way.
			If context is given, it is assumed to be defined vertically (so we transparently horizontalize it during this operation).
			exbgf:factor-out(s,c,t) = xbgf:factor ( choice(s c1 t, s c2 t, ...), sequence(s c t) )
			
			Possibly includes exbgf:massageC2O
		-->
		<xsl:choose>
			<xsl:when test="context and not(optional)">
				<xbgf:horizontal>
					<nonterminal>
						<xsl:value-of select="context"/>
					</nonterminal>
				</xbgf:horizontal>
				<xsl:message>[EXBGF] factor-out ::= horizontal + factor + vertical</xsl:message>
			</xsl:when>
			<xsl:when test="context and optional">
				<xbgf:horizontal>
					<nonterminal>
						<xsl:value-of select="context"/>
					</nonterminal>
				</xbgf:horizontal>
				<xsl:message>[EXBGF] factor-out ::= horizontal + factor + massage + vertical</xsl:message>
			</xsl:when>
			<xsl:when test="not(context) and optional">
				<xsl:message>[EXBGF] factor-out ::= factor + massage</xsl:message>
			</xsl:when>
			<xsl:otherwise>
				<xsl:message>[EXBGF] factor-out ::= factor</xsl:message>
			</xsl:otherwise>
		</xsl:choose>
		<xbgf:factor>
			<bgf:expression>
				<choice>
					<xsl:for-each select="choice/*">
						<bgf:expression>
							<sequence>
								<xsl:copy-of select="../../start/*"/>
								<xsl:copy-of select="."/>
								<xsl:copy-of select="../../tail/*"/>
							</sequence>
						</bgf:expression>
					</xsl:for-each>
					<xsl:if test="optional">
						<bgf:expression>
							<sequence>
								<xsl:copy-of select="start/*"/>
								<xsl:copy-of select="optional/*"/>
								<xsl:copy-of select="tail/*"/>
							</sequence>
						</bgf:expression>
						<bgf:expression>
							<sequence>
								<xsl:copy-of select="start/*"/>
								<xsl:copy-of select="tail/*"/>
							</sequence>
						</bgf:expression>
					</xsl:if>
				</choice>
			</bgf:expression>
			<bgf:expression>
				<sequence>
					<xsl:copy-of select="start/*"/>
					<xsl:if test="choice">
						<bgf:expression>
							<xsl:copy-of select="choice"/>
						</bgf:expression>
					</xsl:if>
					<xsl:if test="optional">
						<bgf:expression>
							<choice>
								<xsl:copy-of select="optional/*"/>
								<bgf:expression>
									<epsilon/>
								</bgf:expression>
							</choice>
						</bgf:expression>
					</xsl:if>
					<xsl:copy-of select="tail/*"/>
				</sequence>
			</bgf:expression>
		</xbgf:factor>
		<xsl:if test="optional and not(optional/bgf:expression/plus)">
			<xbgf:massage>
				<bgf:expression>
					<choice>
						<xsl:copy-of select="optional/*"/>
						<bgf:expression>
							<epsilon/>
						</bgf:expression>
					</choice>
				</bgf:expression>
				<bgf:expression>
					<xsl:copy-of select="optional"/>
				</bgf:expression>
				<xsl:if test="context">
					<in>
						<nonterminal>
							<xsl:value-of select="context[1]"/>
						</nonterminal>
					</in>
				</xsl:if>
			</xbgf:massage>
		</xsl:if>
		<xsl:if test="optional and optional/bgf:expression/plus">
			<xbgf:massage>
				<bgf:expression>
					<choice>
						<xsl:copy-of select="optional/*"/>
						<bgf:expression>
							<epsilon/>
						</bgf:expression>
					</choice>
				</bgf:expression>
				<bgf:expression>
					<star>
						<xsl:copy-of select="optional/bgf:expression/plus/*"/>
					</star>
				</bgf:expression>
				<xsl:if test="context">
					<in>
						<nonterminal>
							<xsl:value-of select="context[1]"/>
						</nonterminal>
					</in>
				</xsl:if>
			</xbgf:massage>
		</xsl:if>
		<xsl:if test="context">
			<xbgf:vertical>
				<nonterminal>
					<xsl:value-of select="context"/>
				</nonterminal>
			</xbgf:vertical>
		</xsl:if>
	</xsl:template>
	<xsl:template match="exbgf:pull-out">
		<!--
			Factor out an expression and make a new nonterminal out of what has been left.
			equivalent to exbgf:factor-out + exbgf:extractC
		-->
		<xsl:choose>
			<xsl:when test="context">
				<xbgf:horizontal>
					<nonterminal>
						<xsl:value-of select="context"/>
					</nonterminal>
				</xbgf:horizontal>
				<xsl:message>[EXBGF] pull-out ::= horizontal + factor + vertical + extract + vertical</xsl:message>
			</xsl:when>
			<xsl:otherwise>
				<xsl:message>[EXBGF] pull-out ::= factor + extract + vertical</xsl:message>
			</xsl:otherwise>
		</xsl:choose>
		<xbgf:factor>
			<bgf:expression>
				<choice>
					<xsl:for-each select="choice/*">
						<bgf:expression>
							<sequence>
								<xsl:copy-of select="../../start/*"/>
								<xsl:copy-of select="."/>
								<xsl:copy-of select="../../tail/*"/>
							</sequence>
						</bgf:expression>
					</xsl:for-each>
				</choice>
			</bgf:expression>
			<bgf:expression>
				<sequence>
					<xsl:copy-of select="start/*"/>
					<bgf:expression>
						<xsl:copy-of select="choice"/>
					</bgf:expression>
					<xsl:copy-of select="tail/*"/>
				</sequence>
			</bgf:expression>
		</xbgf:factor>
		<xsl:if test="context">
			<xbgf:vertical>
				<nonterminal>
					<xsl:value-of select="context"/>
				</nonterminal>
			</xbgf:vertical>
		</xsl:if>
		<xbgf:extract>
			<bgf:production>
				<xsl:copy-of select="nonterminal"/>
				<bgf:expression>
					<xsl:copy-of select="choice"/>
				</bgf:expression>
			</bgf:production>
			<xsl:copy-of select="in[1]"/>
		</xbgf:extract>
		<xbgf:vertical>
			<xsl:copy-of select="nonterminal"/>
		</xbgf:vertical>
	</xsl:template>
</xsl:stylesheet>
