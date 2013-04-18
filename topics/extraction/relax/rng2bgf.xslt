<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:rnga="http://relaxng.org/ns/compatibility/annotations/1.0" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/rng:grammar">
		<bgf:grammar>
			<xsl:apply-templates select="rng:*"/>
		</bgf:grammar>
	</xsl:template>
	<xsl:template match="rng:start">
		<xsl:for-each select="rng:ref|rng:choice/rng:ref">
			<root>
				<xsl:value-of select="@name"/>
			</root>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="rng:include">
		<xsl:message>
			<xsl:text>Don’t forget to merge this with the grammar extracted from the imported module </xsl:text>
			<xsl:value-of select="@href"/>
		</xsl:message>
	</xsl:template>
	<xsl:template match="rng:define">
		<!-- NB: any @combine modifiers are ignored here -->
		<bgf:production>
			<nonterminal>
				<xsl:value-of select="@name"/>
			</nonterminal>
			<xsl:choose>
				<xsl:when test="count(rng:*)=1">
					<xsl:apply-templates select="rng:*"/>
				</xsl:when>
				<xsl:otherwise>
					<bgf:expression>
						<sequence>
							<xsl:apply-templates select="rng:*"/>
						</sequence>
					</bgf:expression>
				</xsl:otherwise>
			</xsl:choose>
		</bgf:production>
	</xsl:template>
	<xsl:template match="rng:element">
		<xsl:choose>
			<xsl:when test="@name">
				<bgf:expression>
					<selectable>
						<selector>
							<xsl:value-of select="@name"/>
						</selector>
						<xsl:choose>
							<xsl:when test="count(rng:*)=1">
								<xsl:apply-templates select="rng:*"/>
							</xsl:when>
							<xsl:otherwise>
								<bgf:expression>
									<sequence>
										<xsl:apply-templates select="rng:*"/>
									</sequence>
								</bgf:expression>
							</xsl:otherwise>
						</xsl:choose>
					</selectable>
				</bgf:expression>
			</xsl:when>
			<xsl:otherwise>
				<xsl:choose>
					<xsl:when test="count(rng:*)=1">
						<xsl:apply-templates select="rng:*"/>
					</xsl:when>
					<xsl:otherwise>
						<bgf:expression>
							<sequence>
								<xsl:apply-templates select="rng:*"/>
							</sequence>
						</bgf:expression>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="rng:interleave">
		<!-- NB: not entirely correct, but good for mapping abstract syntaxes (see also xbgf:permute operator) -->
		<bgf:expression>
			<sequence>
				<xsl:apply-templates select="rng:*"/>
			</sequence>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:group">
		<!-- NB: groups only make sense inside interleaved constructs -->
		<bgf:expression>
			<sequence>
				<xsl:apply-templates select="rng:*"/>
			</sequence>
		</bgf:expression>
	</xsl:template>
	<!-- NB: ignore all annotations -->
	<xsl:template match="rnga:*"/>
	<!-- NB: "any name" means "any selector", which is the default option in BGF anyway -->
	<xsl:template match="rng:anyName"/>
	<xsl:template match="rng:empty">
		<bgf:expression>
			<epsilon/>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:notAllowed">
		<bgf:expression>
			<empty/>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:ref">
		<bgf:expression>
			<nonterminal>
				<xsl:value-of select="@name"/>
			</nonterminal>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:value">
		<bgf:expression>
			<terminal>
				<xsl:value-of select="."/>
			</terminal>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:text|rng:data[@type='string' or @type='ID' or @type='IDREF' or @type='IDREFS' or @type='QName' or @type='NMTOKEN' or @type='NMTOKENS']">
		<bgf:expression>
			<value>string</value>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:data[@type='language' or @type='anyURI']">
		<!-- NB: BGF does not have these built-in, but they are [restricted] strings anyway -->
		<!-- PS: this is not as horrible as it sounds: e.g., the RELAX NG grammar of XHTML models FPI, Content Types and Datetime as strings as well -->
		<bgf:expression>
			<value>string</value>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:data[@type='decimal' or @type='integer' or @type='nonNegativeInteger' or @type='positiveInteger']">
		<!-- NB: details (total digits, fraction digits, etc) are abstracted -->
		<bgf:expression>
			<value>int</value>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:oneOrMore">
		<bgf:expression>
			<plus>
				<xsl:choose>
					<xsl:when test="count(rng:*)=1">
						<xsl:apply-templates select="*"/>
					</xsl:when>
					<xsl:otherwise>
						<bgf:expression>
							<sequence>
								<xsl:apply-templates select="*"/>
							</sequence>
						</bgf:expression>
					</xsl:otherwise>
				</xsl:choose>
			</plus>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:zeroOrMore">
		<bgf:expression>
			<star>
				<xsl:choose>
					<xsl:when test="count(rng:*)=1">
						<xsl:apply-templates select="*"/>
					</xsl:when>
					<xsl:otherwise>
						<bgf:expression>
							<sequence>
								<xsl:apply-templates select="*"/>
							</sequence>
						</bgf:expression>
					</xsl:otherwise>
				</xsl:choose>
			</star>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:optional">
		<bgf:expression>
			<optional>
				<xsl:choose>
					<xsl:when test="count(rng:*)=1">
						<xsl:apply-templates select="*"/>
					</xsl:when>
					<xsl:otherwise>
						<bgf:expression>
							<sequence>
								<xsl:apply-templates select="*"/>
							</sequence>
						</bgf:expression>
					</xsl:otherwise>
				</xsl:choose>
			</optional>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:choice">
		<bgf:expression>
			<choice>
				<xsl:apply-templates select="*"/>
			</choice>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:attribute">
		<bgf:expression>
			<xsl:choose>
				<xsl:when test="rng:anyName">
					<any/>
				</xsl:when>
				<xsl:when test="rng:choice">
					<choice>
						<xsl:apply-templates select="rng:choice/*"/>
					</choice>
				</xsl:when>
				<xsl:when test="rng:text or rng:data/@type='string' or rng:data/@type='ID' or rng:data/@type='QName' or rng:data/@type='NCName'">
					<selectable>
						<selector>
							<xsl:value-of select="@name"/>
						</selector>
						<bgf:expression>
							<value>string</value>
						</bgf:expression>
					</selectable>
				</xsl:when>
				<xsl:when test="rng:data/@type='decimal' or rng:data/@type='integer' or rng:data/@type='nonNegativeInteger' or rng:data/@type='positiveInteger'">
					<selectable>
						<selector>
							<xsl:value-of select="@name"/>
						</selector>
						<bgf:expression>
							<value>int</value>
						</bgf:expression>
					</selectable>
				</xsl:when>
				<xsl:when test="rng:data/@type='anyURI'">
					<!-- NB: relaxation of URI to a string, since BGF does not have a built-in URI type -->
					<selectable>
						<selector>
							<xsl:value-of select="@name"/>
						</selector>
						<bgf:expression>
							<value>string</value>
						</bgf:expression>
					</selectable>
				</xsl:when>
				<xsl:when test="rng:data/@type='IDREF' or rng:data/@type='IDREFS'">
					<!-- NB: uniqueness is not expressed in BGF -->
					<selectable>
						<selector>
							<xsl:value-of select="@name"/>
						</selector>
						<bgf:expression>
							<value>string</value>
						</bgf:expression>
					</selectable>
				</xsl:when>
				<xsl:when test="rng:data/@type='ENTITY' or rng:data/@type='NMTOKEN' or rng:data/@type='NMTOKENS'">
					<!-- NB: could possibly be contemplated to be replaced with a list of entities -->
					<selectable>
						<selector>
							<xsl:value-of select="@name"/>
						</selector>
						<bgf:expression>
							<value>string</value>
						</bgf:expression>
					</selectable>
				</xsl:when>
				<xsl:when test="rng:data/@type='boolean'">
					<!-- NB: fake it [with terminals] till you make it [a part of BGF] -->
					<selectable>
						<selector>
							<xsl:value-of select="@name"/>
						</selector>
						<bgf:expression>
							<choice>
								<bgf:expression>
									<terminal>true</terminal>
								</bgf:expression>
								<bgf:expression>
									<terminal>false</terminal>
								</bgf:expression>
							</choice>
						</bgf:expression>
					</selectable>
				</xsl:when>
				<xsl:when test="rng:ref">
					<selectable>
						<selector>
							<xsl:value-of select="@name"/>
						</selector>
						<bgf:expression>
							<nonterminal>
								<xsl:value-of select="rng:ref/@name"/>
							</nonterminal>
						</bgf:expression>
					</selectable>
				</xsl:when>
				<xsl:when test="rng:choice">
					<selectable>
						<selector>
							<xsl:value-of select="@name"/>
						</selector>
						<bgf:expression>
							<choice>
								<xsl:for-each select="rng:choice/rng:value">
									<bgf:expression>
										<terminal>
											<xsl:value-of select="."/>
										</terminal>
									</bgf:expression>
								</xsl:for-each>
							</choice>
						</bgf:expression>
					</selectable>
				</xsl:when>
				<xsl:when test="rng:value">
					<selectable>
						<selector>
							<xsl:value-of select="@name"/>
						</selector>
						<bgf:expression>
							<terminal>
								<xsl:value-of select="rng:value"/>
							</terminal>
						</bgf:expression>
					</selectable>
				</xsl:when>
				<xsl:when test="count(*)=0 and @name">
					<!-- NB: the logic is simple: if no type is given, the attribute is a string! -->
					<!-- (does not work for elements) -->
					<selectable>
						<selector>
							<xsl:value-of select="@name"/>
						</selector>
						<bgf:expression>
							<value>string</value>
						</bgf:expression>
					</selectable>
				</xsl:when>
				<xsl:otherwise>
					<!-- NB: the following mapping is incorrect yet useful for debugging! -->
					<terminal>
						<xsl:text>ATTRUNKNOWN </xsl:text>
						<xsl:value-of select="local-name(*)"/>
					</terminal>
				</xsl:otherwise>
			</xsl:choose>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="*">
		<bgf:expression>
			<terminal>
				<xsl:text>UNKNOWN </xsl:text>
				<xsl:value-of select="local-name()"/>
			</terminal>
		</bgf:expression>
	</xsl:template>
</xsl:stylesheet>
