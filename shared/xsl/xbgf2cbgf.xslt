<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xbgf="http://planet-sl.org/xbgf" xmlns:cbgf="http://planet-sl.org/cbgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="xml" encoding="UTF-8"/>
	<xsl:template match="/xbgf:sequence">
		<cbgf:relationship>
			<xsl:apply-templates select="*"/>
		</cbgf:relationship>
	</xsl:template>
	<xsl:template match="xbgf:add">
		<cbgf:add-remove>
			<xsl:copy-of select="*"/>
		</cbgf:add-remove>
	</xsl:template>
	<xsl:template match="xbgf:remove">
		<cbgf:remove-add>
			<xsl:copy-of select="*"/>
		</cbgf:remove-add>
	</xsl:template>
	<xsl:template match="xbgf:appear">
		<cbgf:appear-disappear>
			<xsl:copy-of select="*"/>
		</cbgf:appear-disappear>
	</xsl:template>
	<xsl:template match="xbgf:disappear">
		<cbgf:disappear-appear>
			<xsl:copy-of select="*"/>
		</cbgf:disappear-appear>
	</xsl:template>
	<xsl:template match="xbgf:chain">
		<cbgf:chain-unchain>
			<xsl:copy-of select="*"/>
		</cbgf:chain-unchain>
	</xsl:template>
	<xsl:template match="xbgf:unchain">
		<cbgf:unchain-chain>
			<xsl:copy-of select="*"/>
		</cbgf:unchain-chain>
	</xsl:template>
	<xsl:template match="xbgf:anonymize">
		<cbgf:anonymize-deanonymize>
			<xsl:copy-of select="*"/>
		</cbgf:anonymize-deanonymize>
	</xsl:template>
	<xsl:template match="xbgf:deanonymize">
		<cbgf:deanonymize-anonymize>
			<xsl:copy-of select="*"/>
		</cbgf:deanonymize-anonymize>
	</xsl:template>
	<xsl:template match="xbgf:define">
		<cbgf:define-undefine>
			<xsl:copy-of select="*"/>
		</cbgf:define-undefine>
	</xsl:template>
	<xsl:template match="xbgf:undefine">
		<cbgf:undefine-define>
			<todo>
				<xsl:copy-of select="*"/>
			</todo>
		</cbgf:undefine-define>
	</xsl:template>
	<xsl:template match="xbgf:downgrade">
		<cbgf:downgrade-upgrade>
			<xsl:copy-of select="*"/>
		</cbgf:downgrade-upgrade>
	</xsl:template>
	<xsl:template match="xbgf:upgrade">
		<cbgf:upgrade-downgrade>
			<xsl:copy-of select="*"/>
		</cbgf:upgrade-downgrade>
	</xsl:template>
	<xsl:template match="xbgf:designate">
		<cbgf:designate-unlabel>
			<xsl:copy-of select="*"/>
		</cbgf:designate-unlabel>
	</xsl:template>
	<xsl:template match="xbgf:unlabel">
		<cbgf:unlabel-designate>
			<todo>
				<xsl:copy-of select="*"/>
			</todo>
		</cbgf:unlabel-designate>
	</xsl:template>
	<xsl:template match="xbgf:deyaccify">
		<cbgf:deyaccify-yaccify>
			<todo>
				<xsl:copy-of select="*"/>
			</todo>
		</cbgf:deyaccify-yaccify>
	</xsl:template>
	<xsl:template match="xbgf:yaccify">
		<cbgf:yaccify-deyaccify>
			<xsl:copy-of select="*"/>
		</cbgf:yaccify-deyaccify>
	</xsl:template>
	<xsl:template match="xbgf:eliminate">
		<cbgf:eliminate-introduce>
			<todo>
				<xsl:copy-of select="*"/>
			</todo>
		</cbgf:eliminate-introduce>
	</xsl:template>
	<xsl:template match="xbgf:introduce">
		<cbgf:introduce-eliminate>
			<xsl:copy-of select="*"/>
		</cbgf:introduce-eliminate>
	</xsl:template>
	<xsl:template match="xbgf:extract">
		<cbgf:extract-inline>
			<xsl:copy-of select="*"/>
		</cbgf:extract-inline>
	</xsl:template>
	<xsl:template match="xbgf:inline">
		<cbgf:inline-extract>
			<todo>
				<xsl:value-of select="."/>
			</todo>
		</cbgf:inline-extract>
	</xsl:template>
	<xsl:template match="xbgf:factor">
		<cbgf:factor-factor>
			<xsl:copy-of select="*"/>
		</cbgf:factor-factor>
	</xsl:template>
	<xsl:template match="xbgf:unfold">
		<cbgf:unfold-fold>
			<xsl:copy-of select="*"/>
		</cbgf:unfold-fold>
	</xsl:template>
	<xsl:template match="xbgf:fold">
		<cbgf:fold-unfold>
			<xsl:copy-of select="*"/>
		</cbgf:fold-unfold>
	</xsl:template>
	<xsl:template match="xbgf:horizontal">
		<cbgf:horizontal-vertical>
			<xsl:copy-of select="nonterminal"/>
		</cbgf:horizontal-vertical>
	</xsl:template>
	<xsl:template match="xbgf:vertical">
		<cbgf:vertical-horizontal>
			<xsl:copy-of select="nonterminal"/>
		</cbgf:vertical-horizontal>
	</xsl:template>
	<xsl:template match="xbgf:inject">
		<cbgf:inject-project>
			<xsl:copy-of select="*"/>
		</cbgf:inject-project>
	</xsl:template>
	<xsl:template match="xbgf:project">
		<cbgf:project-inject>
			<xsl:copy-of select="*"/>
		</cbgf:project-inject>
	</xsl:template>
	<xsl:template match="xbgf:massage">
		<cbgf:massage-massage>
			<xsl:copy-of select="*"/>
		</cbgf:massage-massage>
	</xsl:template>
	<xsl:template match="xbgf:narrow">
		<cbgf:narrow-widen>
			<xsl:copy-of select="*"/>
		</cbgf:narrow-widen>
	</xsl:template>
	<xsl:template match="xbgf:widen">
		<cbgf:widen-narrow>
			<xsl:copy-of select="*"/>
		</cbgf:widen-narrow>
	</xsl:template>
	<xsl:template match="xbgf:permute">
		<cbgf:permute-permute>
			<todo>
				<xsl:copy-of select="*"/>
			</todo>
		</cbgf:permute-permute>
	</xsl:template>
	<xsl:template match="xbgf:rename">
		<cbgf:rename-rename>
			<xsl:copy-of select="*"/>
		</cbgf:rename-rename>
	</xsl:template>
	<xsl:template match="xbgf:reroot">
		<cbgf:reroot-reroot>
			<todo>
				<xsl:copy-of select="*"/>
			</todo>
		</cbgf:reroot-reroot>
	</xsl:template>
	<xsl:template match="xbgf:replace">
		<cbgf:replace-replace>
			<xsl:copy-of select="*"/>
		</cbgf:replace-replace>
	</xsl:template>
	<xsl:template match="xbgf:abridge">
		<cbgf:abridge-detour>
			<xsl:copy-of select="*"/>
		</cbgf:abridge-detour>
	</xsl:template>
	<xsl:template match="xbgf:detour">
		<cbgf:detour-abridge>
			<xsl:copy-of select="*"/>
		</cbgf:detour-abridge>
	</xsl:template>
	<xsl:template match="xbgf:concretize">
		<cbgf:concretize-abstractize>
			<xsl:copy-of select="*"/>
		</cbgf:concretize-abstractize>
	</xsl:template>
	<xsl:template match="xbgf:abstractize">
		<cbgf:abstractize-concretize>
			<xsl:copy-of select="*"/>
		</cbgf:abstractize-concretize>
	</xsl:template>
	<xsl:template match="xbgf:rassoc|xbgf:lassoc">
		<cbgf:assoc-iterate>
			<xsl:choose>
				<xsl:when test="count(bgf:production/bgf:expression/sequence/*)=3">
					<bgf:production>
						<xsl:copy-of select="bgf:production/label"/>
						<xsl:copy-of select="bgf:production/nonterminal"/>
						<bgf:expression>
							<sequence>
								<xsl:copy-of select="bgf:production/bgf:expression/sequence/bgf:expression[1]"/>
								<bgf:expression>
									<star>
										<bgf:expression>
											<sequence>
												<xsl:copy-of select="bgf:production/bgf:expression/sequence/bgf:expression[2]"/>
												<xsl:copy-of select="bgf:production/bgf:expression/sequence/bgf:expression[3]"/>
											</sequence>
										</bgf:expression>
									</star>
								</bgf:expression>
							</sequence>
						</bgf:expression>
					</bgf:production>
				</xsl:when>
				<xsl:when test="count(bgf:production/bgf:expression/sequence/*)=2">
					<bgf:production>
						<xsl:copy-of select="bgf:production/label"/>
						<xsl:copy-of select="bgf:production/nonterminal"/>
						<bgf:expression>
							<plus>
								<xsl:copy-of select="bgf:production/bgf:expression/sequence/bgf:expression[1]"/>
							</plus>
						</bgf:expression>
					</bgf:production>
				</xsl:when>
				<xsl:otherwise>
					<todo>UNKNOWN ASSOC PATTERN</todo>
				</xsl:otherwise>
			</xsl:choose>
			<xsl:copy-of select="bgf:production"/>
		</cbgf:assoc-iterate>
	</xsl:template>
	<xsl:template match="xbgf:equate">
		<cbgf:equate-clone>
			<todo>
				<xsl:copy-of select="*"/>
			</todo>
		</cbgf:equate-clone>
	</xsl:template>
	<xsl:template match="xbgf:unite">
		<cbgf:unite-split>
			<todo>
				<xsl:copy-of select="*"/>
			</todo>
		</cbgf:unite-split>
	</xsl:template>
</xsl:stylesheet>
