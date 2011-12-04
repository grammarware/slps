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
			<xsl:copy-of select="bgf:expression[2]"/>
			<xsl:copy-of select="bgf:expression[1]"/>
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
</xsl:stylesheet>
