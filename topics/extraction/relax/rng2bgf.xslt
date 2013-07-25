<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:rnga="http://relaxng.org/ns/compatibility/annotations/1.0" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/rng:grammar">
		<bgf:grammar>
			<!-- NB: in BGF, root nonterminals are always defined before all the production rules -->
			<xsl:apply-templates select="rng:start"/>
			<xsl:apply-templates select="rng:*[local-name()!='start']"/>
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
		<xsl:apply-templates select="*"/>
	</xsl:template>
	<xsl:template match="rng:define">
		<xsl:if test="rng:element/rng:grammar">
			<xsl:apply-templates select="rng:element/rng:grammar/rng:define"/>
		</xsl:if>
		<!-- NB: any @combine modifiers are ignored here -->
		<bgf:production>
			<nonterminal>
				<xsl:value-of select="@name"/>
			</nonterminal>
			<xsl:choose>
				<xsl:when test="rng:element/rng:grammar">
					<xsl:apply-templates select="rng:element/rng:grammar/rng:start/*"/>
				</xsl:when>
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
			<xsl:when test="rng:choice[rng:name]">
				<bgf:expression>
					<choice>
						<xsl:for-each select="rng:choice/rng:name">
							<bgf:expression>
								<selectable>
									<selector>
										<xsl:value-of select="."/>
									</selector>
									<xsl:if test="count(../../*[not(local-name()='choice' and rng:name)])=1">
										<xsl:apply-templates select="../../*[not(local-name()='choice' and rng:name)]"/>
									</xsl:if>
									<xsl:if test="count(../../*[not(local-name()='choice' and rng:name)]) &gt; 1">
										<bgf:expression>
											<sequence>
												<xsl:apply-templates select="../../*[not(local-name()='choice' and rng:name)]"/>
											</sequence>
										</bgf:expression>
									</xsl:if>
								</selectable>
							</bgf:expression>
						</xsl:for-each>
					</choice>
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
	<xsl:template match="rng:list">
		<!-- NB: this mapping is correct, but it is an abstraction -->
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
	<!-- NB: no advanced namespace support in BGF -->
	<xsl:template match="rng:nsName"/>
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
	<xsl:template match="rng:mixed">
		<bgf:expression>
			<xsl:choose>
				<xsl:when test="rng:zeroOrMore">
					<star>
						<bgf:expression>
							<choice>
								<bgf:expression>
									<value>string</value>
								</bgf:expression>
								<xsl:apply-templates select="rng:zeroOrMore/*"/>
							</choice>
						</bgf:expression>
					</star>
				</xsl:when>
				<xsl:when test="rng:oneOrMore">
					<plus>
						<bgf:expression>
							<choice>
								<bgf:expression>
									<value>string</value>
								</bgf:expression>
								<xsl:apply-templates select="rng:oneOrMore/*"/>
							</choice>
						</bgf:expression>
					</plus>
				</xsl:when>
				<xsl:when test="rng:ref">
					<sequence>
						<bgf:expression>
							<optional>
								<bgf:expression>
									<value>string</value>
								</bgf:expression>
							</optional>
						</bgf:expression>
						<xsl:apply-templates select="rng:ref"/>
						<bgf:expression>
							<optional>
								<bgf:expression>
									<value>string</value>
								</bgf:expression>
							</optional>
						</bgf:expression>
					</sequence>
				</xsl:when>
				<xsl:otherwise>
					<bgf:expression>
						<!-- NB: other variants not treated -->
						<terminal>UNKNOWNmixed</terminal>
					</bgf:expression>
				</xsl:otherwise>
			</xsl:choose>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:text|rng:data[@type='string' or @type='ID' or @type='IDREF' or @type='IDREFS' or @type='QName' or @type='NCName'
		or @type='normalizedString']">
		<bgf:expression>
			<value>string</value>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:data[@type='language' or @type='anyURI' or @type='token' or @type='date' or @type='dateTime' or @type='time'
		or @type='base64Binary' or @type='duration']">
		<!-- NB: BGF does not have these built-in, but they are [restricted] strings anyway -->
		<!-- PS: this is not as horrible as it sounds: e.g., the RELAX NG grammar of XHTML models FPI, Content Types and Datetime as strings as well -->
		<bgf:expression>
			<value>string</value>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:data[@type='ENTITY' or @type='NMTOKEN' or @type='NMTOKENS']">
		<!-- NB: could possibly be contemplated to be replaced with a list of entities -->
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
	<xsl:template match="rng:data[@type='float' or @type='double']">
		<!-- NB: floats could possibly be considered as some combinations of integers and dots -->
		<bgf:expression>
			<value>string</value>
		</bgf:expression>
	</xsl:template>
	<xsl:template match="rng:data[@type='boolean']">
		<!-- NB: fake it [with terminals] till you make it [a part of BGF] -->
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
		<xsl:choose>
			<xsl:when test="rng:anyName and count(*)=1">
				<!-- NB: if the only known thing is that it can have any name, then it can be anything -->
				<bgf:expression>
					<any/>
				</bgf:expression>
			</xsl:when>
			<xsl:when test="count(*)=0 and (@name or rng:name)">
				<!-- NB: the logic is simple: if no type is given, the attribute is a string! -->
				<!-- (does not work for elements) -->
				<bgf:expression>
					<selectable>
						<selector>
							<xsl:value-of select="@name|rng:name"/>
						</selector>
						<bgf:expression>
							<value>string</value>
						</bgf:expression>
					</selectable>
				</bgf:expression>
			</xsl:when>
			<xsl:when test="@name|rng:name">
				<bgf:expression>
					<selectable>
						<selector>
							<xsl:value-of select="@name|rng:name"/>
						</selector>
						<xsl:apply-templates select="rng:*[local-name()!='name']"/>
					</selectable>
				</bgf:expression>
			</xsl:when>
			<xsl:when test="rng:choice[rng:name]">
				<bgf:expression>
					<choice>
						<xsl:for-each select="rng:choice/rng:name">
							<bgf:expression>
								<selectable>
									<selector>
										<xsl:value-of select="."/>
									</selector>
									<xsl:if test="count(../../*[not(local-name()='choice' and rng:name)])=1">
										<xsl:apply-templates select="../../*[not(local-name()='choice' and rng:name)]"/>
									</xsl:if>
									<xsl:if test="count(../../*[not(local-name()='choice' and rng:name)]) &gt; 1">
										<bgf:expression>
											<sequence>
												<xsl:apply-templates select="../../*[not(local-name()='choice' and rng:name)]"/>
											</sequence>
										</bgf:expression>
									</xsl:if>
								</selectable>
							</bgf:expression>
						</xsl:for-each>
					</choice>
				</bgf:expression>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates select="*"/>
				<!-- NB: the following mapping is incorrect yet useful for debugging! -->
				<!-- <terminal>
						<xsl:text>ATTRUNKNOWN </xsl:text>
						<xsl:value-of select="local-name(*)"/>
					</terminal> -->
			</xsl:otherwise>
		</xsl:choose>
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
