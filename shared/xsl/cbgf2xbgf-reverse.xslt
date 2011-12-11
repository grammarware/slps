<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:cbgf="http://planet-sl.org/cbgf" xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/cbgf:relationship">
		<xbgf:sequence>
			<xsl:for-each select="*">
				<xsl:sort select="position()" data-type="number" order="descending"/>
				<xsl:apply-templates select="."/>
			</xsl:for-each>
		</xbgf:sequence>
	</xsl:template>
	<xsl:template match="cbgf:remove-add">
		<xbgf:add>
			<xsl:copy-of select="*"/>
		</xbgf:add>
	</xsl:template>
	<xsl:template match="cbgf:add-remove">
		<xbgf:remove>
			<xsl:copy-of select="*"/>
		</xbgf:remove>
	</xsl:template>
	<xsl:template match="cbgf:disappear-appear">
		<xbgf:appear>
			<xsl:copy-of select="*"/>
		</xbgf:appear>
	</xsl:template>
	<xsl:template match="cbgf:appear-disappear">
		<xbgf:disappear>
			<xsl:copy-of select="*"/>
		</xbgf:disappear>
	</xsl:template>
	<xsl:template match="cbgf:unchain-chain">
		<xbgf:chain>
			<xsl:copy-of select="*"/>
		</xbgf:chain>
	</xsl:template>
	<xsl:template match="cbgf:chain-unchain">
		<xbgf:unchain>
			<xsl:copy-of select="*"/>
		</xbgf:unchain>
	</xsl:template>
	<xsl:template match="cbgf:deanonymize-anonymize">
		<xbgf:anonymize>
			<xsl:copy-of select="*"/>
		</xbgf:anonymize>
	</xsl:template>
	<xsl:template match="cbgf:anonymize-deanonymize">
		<xbgf:deanonymize>
			<xsl:copy-of select="*"/>
		</xbgf:deanonymize>
	</xsl:template>
	<xsl:template match="cbgf:undefine-define">
		<xbgf:define>
			<xsl:copy-of select="*"/>
		</xbgf:define>
	</xsl:template>
	<xsl:template match="cbgf:define-undefine">
		<xbgf:undefine>
			<nonterminal>
				<xsl:value-of select="bgf:production[1]/nonterminal"/>
			</nonterminal>
		</xbgf:undefine>
	</xsl:template>
	<xsl:template match="cbgf:upgrade-downgrade">
		<xbgf:downgrade>
			<xsl:copy-of select="*"/>
		</xbgf:downgrade>
	</xsl:template>
	<xsl:template match="cbgf:downgrade-upgrade">
		<xbgf:upgrade>
			<xsl:copy-of select="*"/>
		</xbgf:upgrade>
	</xsl:template>
	<xsl:template match="cbgf:unlabel-designate">
		<xbgf:designate>
			<xsl:copy-of select="*"/>
		</xbgf:designate>
	</xsl:template>
	<xsl:template match="cbgf:designate-unlabel">
		<xbgf:unlabel>
			<label>
				<xsl:value-of select="bgf:production[1]/label"/>
			</label>
		</xbgf:unlabel>
	</xsl:template>
	<xsl:template match="cbgf:yaccify-deyaccify">
		<xbgf:deyaccify>
			<nonterminal>
				<xsl:value-of select="bgf:production[1]/nonterminal"/>
			</nonterminal>
		</xbgf:deyaccify>
	</xsl:template>
	<xsl:template match="cbgf:deyaccify-yaccify">
		<xbgf:yaccify>
			<xsl:copy-of select="*"/>
		</xbgf:yaccify>
	</xsl:template>
	<xsl:template match="cbgf:introduce-eliminate">
		<xbgf:eliminate>
			<nonterminal>
				<xsl:value-of select="bgf:production[1]/nonterminal"/>
			</nonterminal>
		</xbgf:eliminate>
	</xsl:template>
	<xsl:template match="cbgf:eliminate-introduce">
		<xbgf:introduce>
			<xsl:copy-of select="*"/>
		</xbgf:introduce>
	</xsl:template>
	<xsl:template match="cbgf:inline-extract">
		<xbgf:extract>
			<xsl:copy-of select="*"/>
		</xbgf:extract>
	</xsl:template>
	<xsl:template match="cbgf:extract-inline">
		<xbgf:inline>
			<xsl:value-of select="bgf:production[1]/nonterminal"/>
		</xbgf:inline>
	</xsl:template>
	<xsl:template match="cbgf:factor-factor">
		<xbgf:factor>
			<xsl:copy-of select="bgf:expression[2]"/>
			<xsl:copy-of select="bgf:expression[1]"/>
		</xbgf:factor>
	</xsl:template>
	<xsl:template match="cbgf:fold-unfold">
		<xbgf:unfold>
			<xsl:copy-of select="*"/>
		</xbgf:unfold>
	</xsl:template>
	<xsl:template match="cbgf:unfold-fold">
		<xbgf:fold>
			<xsl:copy-of select="*"/>
		</xbgf:fold>
	</xsl:template>
	<xsl:template match="cbgf:vertical-horizontal">
		<xbgf:horizontal>
			<xsl:copy-of select="nonterminal"/>
		</xbgf:horizontal>
	</xsl:template>
	<xsl:template match="cbgf:horizontal-vertical">
		<xbgf:vertical>
			<xsl:copy-of select="nonterminal"/>
		</xbgf:vertical>
	</xsl:template>
	<xsl:template match="cbgf:iterate-assoc">
		<!-- TODO: left or right? -->
		<xbgf:lassoc>
			<xsl:copy-of select="bgf:production[1]"/>
		</xbgf:lassoc>
	</xsl:template>
	<xsl:template match="cbgf:assoc-iterate">
		<xbgf:remove>
			<vertical>
				<xsl:copy-of select="bgf:production[2]"/>
			</vertical>
		</xbgf:remove>
		<xbgf:add>
			<vertical>
				<xsl:copy-of select="bgf:production[1]"/>
			</vertical>
		</xbgf:add>
	</xsl:template>
	<xsl:template match="cbgf:project-inject">
		<xbgf:inject>
			<xsl:copy-of select="*"/>
		</xbgf:inject>
	</xsl:template>
	<xsl:template match="cbgf:inject-project">
		<xbgf:project>
			<xsl:copy-of select="*"/>
		</xbgf:project>
	</xsl:template>
	<xsl:template match="cbgf:massage-massage">
		<xbgf:massage>
			<xsl:copy-of select="bgf:expression[2]"/>
			<xsl:copy-of select="bgf:expression[1]"/>
		</xbgf:massage>
	</xsl:template>
	<xsl:template match="cbgf:widen-narrow">
		<xbgf:narrow>
			<xsl:copy-of select="bgf:expression[2]"/>
			<xsl:copy-of select="bgf:expression[1]"/>
		</xbgf:narrow>
	</xsl:template>
	<xsl:template match="cbgf:narrow-widen">
		<xbgf:widen>
			<xsl:copy-of select="bgf:expression[2]"/>
			<xsl:copy-of select="bgf:expression[1]"/>
		</xbgf:widen>
	</xsl:template>
	<xsl:template match="cbgf:permute-permute">
		<xbgf:permute>
			<xsl:copy-of select="bgf:production[1]"/>
		</xbgf:permute>
	</xsl:template>
	<xsl:template match="cbgf:rename-rename">
		<!-- TODO: does not work with scope (as many other things as well). Don't try this at home or at work! -->
		<xbgf:rename>
			<xsl:element name="{local-name(*)}">
				<from>
					<xsl:value-of select="*/to"/>
				</from>
				<to>
					<xsl:value-of select="*/from"/>
				</to>
			</xsl:element>
		</xbgf:rename>
	</xsl:template>
	<xsl:template match="cbgf:reroot-reroot">
		<xbgf:reroot>
			<xsl:copy-of select="from/*"/>
		</xbgf:reroot>
	</xsl:template>
	<xsl:template match="cbgf:replace-replace">
		<xbgf:replace>
			<xsl:copy-of select="bgf:expression[2]"/>
			<xsl:copy-of select="bgf:expression[1]"/>
		</xbgf:replace>
	</xsl:template>
	<xsl:template match="cbgf:detour-abridge">
		<xbgf:abridge>
			<xsl:copy-of select="*"/>
		</xbgf:abridge>
	</xsl:template>
	<xsl:template match="cbgf:abridge-detour">
		<xbgf:detour>
			<xsl:copy-of select="*"/>
		</xbgf:detour>
	</xsl:template>
	<xsl:template match="cbgf:abstractize-concretize">
		<xbgf:concretize>
			<xsl:copy-of select="*"/>
		</xbgf:concretize>
	</xsl:template>
	<xsl:template match="cbgf:concretize-abstractize">
		<xbgf:abstractize>
			<xsl:copy-of select="*"/>
		</xbgf:abstractize>
	</xsl:template>
	<xsl:template match="cbgf:split-unite">
		<xbgf:unite>
			<add>
				<xsl:value-of select="add/bgf:production[1]/nonterminal"/>
			</add>
			<to>
				<xsl:value-of select="to/bgf:production[1]/nonterminal"/>
			</to>
		</xbgf:unite>
	</xsl:template>
	<xsl:template match="cbgf:unite-split">
		<xsl:variable name="n1" select="add/bgf:production[1]/nonterminal"/>
		<xsl:variable name="n2" select="to/bgf:production[1]/nonterminal"/>
		<xbgf:introduce>
			<xsl:copy-of select="add/bgf:production"/>
		</xbgf:introduce>
		<xsl:for-each select="add/bgf:production">
			<xbgf:remove>
				<vertical>
					<xsl:call-template name="copy-structure-rename-nonterminal">
						<xsl:with-param name="structure" select="."/>
						<xsl:with-param name="replace" select="$n1"/>
						<xsl:with-param name="with" select="$n2"/>
					</xsl:call-template>
				</vertical>
			</xbgf:remove>
		</xsl:for-each>
		<xsl:for-each select="in/*">
			<xbgf:replace>
				<bgf:expression>
					<nonterminal>
						<xsl:value-of select="$n2"/>
					</nonterminal>
				</bgf:expression>
				<bgf:expression>
					<nonterminal>
						<xsl:value-of select="$n1"/>
					</nonterminal>
				</bgf:expression>
				<in>
					<xsl:copy-of select="."/>
				</in>
			</xbgf:replace>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="cbgf:clone-equate">
		<xbgf:equate>
			<align>
				<xsl:value-of select="align/bgf:production[1]/nonterminal"/>
			</align>
			<with>
				<xsl:value-of select="with"/>
			</with>
		</xbgf:equate>
	</xsl:template>
	<xsl:template match="cbgf:equate-clone">
		<xsl:variable name="n1" select="align/bgf:production[1]/nonterminal"/>
		<xsl:variable name="n2" select="with"/>
		<xbgf:introduce>
			<xsl:copy-of select="align/bgf:production"/>
		</xbgf:introduce>
		<xsl:for-each select="in/*">
			<xbgf:replace>
				<bgf:expression>
					<nonterminal>
						<xsl:value-of select="$n2"/>
					</nonterminal>
				</bgf:expression>
				<bgf:expression>
					<nonterminal>
						<xsl:value-of select="$n1"/>
					</nonterminal>
				</bgf:expression>
				<in>
					<xsl:copy-of select="."/>
				</in>
			</xbgf:replace>
		</xsl:for-each>
	</xsl:template>
	<xsl:template name="copy-structure-rename-nonterminal">
		<xsl:param name="structure"/>
		<xsl:param name="replace"/>
		<xsl:param name="with"/>
		<xsl:choose>
			<xsl:when test="local-name($structure)='nonterminal' and . = $replace">
				<nonterminal>
					<xsl:value-of select="$with"/>
				</nonterminal>
			</xsl:when>
			<xsl:when test="name($structure)=local-name($structure) and (local-name($structure)!='nonterminal' or . != $replace)">
				<xsl:element name="{local-name($structure)}">
					<xsl:value-of select="text()"/>
					<xsl:for-each select="$structure/*">
						<xsl:call-template name="copy-structure-rename-nonterminal">
							<xsl:with-param name="structure" select="."/>
							<xsl:with-param name="replace" select="$replace"/>
							<xsl:with-param name="with" select="$with"/>
						</xsl:call-template>
					</xsl:for-each>
				</xsl:element>
			</xsl:when>
			<xsl:otherwise>
				<xsl:element name="bgf:{local-name($structure)}">
					<xsl:for-each select="$structure/*">
						<xsl:call-template name="copy-structure-rename-nonterminal">
							<xsl:with-param name="structure" select="."/>
							<xsl:with-param name="replace" select="$replace"/>
							<xsl:with-param name="with" select="$with"/>
						</xsl:call-template>
					</xsl:for-each>
				</xsl:element>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
