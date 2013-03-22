<?xml version="1.0" encoding="UTF-8"?>
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
			<!-- we have no way of expressing this in BGF (yet) -->
			<xsl:when test="type/typeSpec/opt_typeModifier/typeModifier='not'"/>
			<xsl:when test="type/typeSpec/opt_typeModifier/typeModifier='see'"/>
			<xsl:when test="type/typeSpec/opt_typeModifier/typeModifier='push'"/>
			<xsl:when test="type/typeSpec/opt_typeModifier/typeModifier='pop'"/>
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
			<!-- we assume that [repeat x] means x* -->
			<xsl:when test="type/typeSpec/opt_typeRepeater/typeRepeater='*' or type/typeSpec/opt_typeModifier/typeModifier='repeat'">
				<bgf:expression>
					<star>
						<bgf:expression>
							<xsl:if test="type/typeSpec/typeid/id">
								<nonterminal>
									<xsl:value-of select="type/typeSpec/typeid/id"/>
								</nonterminal>
							</xsl:if>
							<xsl:if test="type/typeSpec/typeid/literal">
								<terminal>
									<xsl:value-of select="substring(type/typeSpec/typeid/literal/unquotedLiteral/special,2)"/>
								</terminal>
							</xsl:if>
						</bgf:expression>
					</star>
				</bgf:expression>
			</xsl:when>
			<!-- comma-separated lists! -->
			<xsl:when test="type/typeSpec/opt_typeRepeater/typeRepeater=',' or type/typeSpec/opt_typeModifier/typeModifier='list'">
				<bgf:expression>
					<sepliststar>
						<bgf:expression>
							<nonterminal>
								<xsl:value-of select="type/typeSpec/typeid/id"/>
							</nonterminal>
						</bgf:expression>
						<bgf:expression>
							<terminal>,</terminal>
						</bgf:expression>
					</sepliststar>
				</bgf:expression>
			</xsl:when>
			<xsl:when test="type/typeSpec/opt_typeRepeater/typeRepeater=',+'">
				<bgf:expression>
					<seplistplus>
						<bgf:expression>
							<nonterminal>
								<xsl:value-of select="type/typeSpec/typeid/id"/>
							</nonterminal>
						</bgf:expression>
						<bgf:expression>
							<terminal>,</terminal>
						</bgf:expression>
					</seplistplus>
				</bgf:expression>
			</xsl:when>
			<xsl:when test="type/typeSpec/opt_typeRepeater/typeRepeater='?' or type/typeSpec/opt_typeModifier/typeModifier='opt'">
				<xsl:if test="type/typeSpec/typeid/id">
					<bgf:expression>
						<optional>
							<bgf:expression>
								<nonterminal>
									<xsl:value-of select="type/typeSpec/typeid/id"/>
								</nonterminal>
							</bgf:expression>
						</optional>
					</bgf:expression>
				</xsl:if>
				<xsl:if test="type/typeSpec/typeid/literal">
					<bgf:expression>
						<optional>
							<xsl:apply-templates select="type/typeSpec/typeid/literal"/>
						</optional>
					</bgf:expression>
				</xsl:if>
			</xsl:when>
			<xsl:when test="type/typeSpec/typeid/literal and substring(type/typeSpec/typeid/literal/unquotedLiteral/special, string-length(type/typeSpec/typeid/literal/unquotedLiteral/special) )='?'">
				<bgf:expression>
					<optional>
						<bgf:expression>
							<terminal>
								<xsl:value-of select="substring(type/typeSpec/typeid/literal/unquotedLiteral/special, 2, string-length(type/typeSpec/typeid/literal/unquotedLiteral/special)-2)"/>
							</terminal>
						</bgf:expression>
					</optional>
				</bgf:expression>
			</xsl:when>
			<xsl:when test="literal">
				<xsl:apply-templates select="literal"/>
			</xsl:when>
			<xsl:when test="type/typeSpec/typeid/id = 'empty'">
				<bgf:expression>
					<epsilon/>
				</bgf:expression>
			</xsl:when>
			<xsl:when test="type/typeSpec/typeid/id = 'any'">
				<bgf:expression>
					<any/>
				</bgf:expression>
			</xsl:when>
			<!-- whitespace-related special nonterminals -->
			<xsl:when test="type/formatCues">
				<xsl:apply-templates select="type/formatCues"/>
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
	<xsl:template match="literal">
		<xsl:choose>
			<xsl:when test="comment"/>
			<xsl:when test="quotedLiteral/unquotedLiteral = &quot; '&quot;">
				<bgf:expression>
					<terminal>'</terminal>
				</bgf:expression>
			</xsl:when>
			<xsl:when test="quotedLiteral/unquotedLiteral">
				<bgf:expression>
					<terminal>
						<xsl:value-of select="quotedLiteral/unquotedLiteral"/>
					</terminal>
				</bgf:expression>
			</xsl:when>
			<xsl:when test="substring-after(unquotedLiteral/special,&quot;'&quot;)!=&quot;&quot;">
				<bgf:expression>
					<terminal>
						<xsl:value-of select="substring-after(unquotedLiteral/special,&quot;'&quot;)"/>
					</terminal>
				</bgf:expression>
			</xsl:when>
			<xsl:otherwise>
				<bgf:expression>
					<terminal>
						<xsl:value-of select="unquotedLiteral/*"/>
					</terminal>
				</bgf:expression>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="formatCues">
		<xsl:choose>
			<xsl:when test="count(repeat_formatCue/formatCue) &gt; 1">
				<bgf:expression>
					<sequence>
						<xsl:for-each select="repeat_formatCue/formatCue">
							<xsl:choose>
								<xsl:when test="substring-after(formatId,' ')!=''">
									<bgf:expression>
										<nonterminal>
											<xsl:value-of select="substring-after(formatId,' ')"/>
										</nonterminal>
									</bgf:expression>
								</xsl:when>
								<xsl:otherwise>
									<bgf:expression>
										<nonterminal>
											<xsl:value-of select="formatId"/>
										</nonterminal>
									</bgf:expression>
								</xsl:otherwise>
							</xsl:choose>
						</xsl:for-each>
					</sequence>
				</bgf:expression>
			</xsl:when>
			<xsl:otherwise>
				<xsl:choose>
					<xsl:when test="substring-after(repeat_formatCue/formatCue/formatId,' ')!=''">
						<bgf:expression>
							<nonterminal>
								<xsl:value-of select="substring-after(repeat_formatCue/formatCue/formatId,' ')"/>
							</nonterminal>
						</bgf:expression>
					</xsl:when>
					<xsl:otherwise>
						<bgf:expression>
							<nonterminal>
								<xsl:value-of select="repeat_formatCue/formatCue/formatId"/>
							</nonterminal>
						</bgf:expression>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
