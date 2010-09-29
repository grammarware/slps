<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/program">
		<bgf:grammar>
			<xsl:apply-templates select="repeat_statement/statement/functionStatement[ruleid/id='main']"/>
			<xsl:apply-templates select="repeat_statement/statement/defineStatement"/>
		</bgf:grammar>
	</xsl:template>
	
	<xsl:template match="functionStatement">
		<root>
			<xsl:value-of select=".//typeid/id"/>
		</root>
	</xsl:template>
	
	<xsl:template match="defineStatement">
		<bgf:production>
			<nonterminal>
				<xsl:value-of select="typeid/id"/>
			</nonterminal>
			<!--xsl:apply-templates select="repeat_literalOrType/literalOrType/type/typeSpec"/-->
			<xsl:choose>
				<xsl:when test="repeat_barLiteralsAndTypes">
					<bgf:expression>
						<choice>
							<xsl:call-template name="sequenceOrNot">
								<xsl:with-param name="list" select="repeat_literalOrType"/>
							</xsl:call-template>
							<xsl:for-each select="repeat_barLiteralsAndTypes/barLiteralsAndTypes/repeat_literalOrType">
								<xsl:call-template name="sequenceOrNot">
									<xsl:with-param name="list" select="."/>
								</xsl:call-template>
							</xsl:for-each>
						</choice>
					</bgf:expression>
				</xsl:when>
				<xsl:otherwise>
					<xsl:call-template name="sequenceOrNot">
						<xsl:with-param name="list" select="repeat_literalOrType"/>
					</xsl:call-template>
				</xsl:otherwise>
			</xsl:choose>
		</bgf:production>
	</xsl:template>
	
	<xsl:template name="sequenceOrNot">
		<xsl:param name="list"/>
		<xsl:choose>
			<xsl:when test="count($list/literalOrType)=1">
				<xsl:apply-templates select="$list/literalOrType"/>
			</xsl:when>
			<xsl:otherwise>
				<bgf:expression>
					<sequence>
						<xsl:apply-templates select="$list/literalOrType"/>
					</sequence>
				</bgf:expression>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<xsl:template match="literalOrType">
	<!--xsl:for-each select="repeat_literalOrType/literalOrType"-->
		<xsl:choose>
			<xsl:when test="type/typeSpec/opt_typeRepeater/typeRepeater='+'">
				<bgf:expression>
					<plus>
						<bgf:expression>
							<nonterminal>
								<xsl:value-of select="type/typeSpec/typeid/id"/>
							</nonterminal>
						</bgf:expression>
					</plus>
				</bgf:expression>
			</xsl:when>
			<xsl:when test="type/typeSpec/opt_typeRepeater/typeRepeater='*'">
				<bgf:expression>
					<star>
						<bgf:expression>
							<nonterminal>
								<xsl:value-of select="type/typeSpec/typeid/id"/>
							</nonterminal>
						</bgf:expression>
					</star>
				</bgf:expression>
			</xsl:when>
			<xsl:when test="type/typeSpec/opt_typeRepeater/typeRepeater='?'">
				<bgf:expression>
					<optional>
						<bgf:expression>
							<nonterminal>
								<xsl:value-of select="type/typeSpec/typeid/id"/>
							</nonterminal>
						</bgf:expression>
					</optional>
				</bgf:expression>
			</xsl:when>
			<xsl:when test="literal">
				<bgf:expression>
					<terminal>
						<xsl:value-of select="literal/unquotedLiteral/*"/>
					</terminal>
				</bgf:expression>
			</xsl:when>
			<!-- fallback -->
			<xsl:otherwise>
				<bgf:expression>
					<nonterminal>
						<xsl:value-of select="type/typeSpec/typeid/id"/>
					</nonterminal>
				</bgf:expression>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
