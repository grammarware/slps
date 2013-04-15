<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:bgf="http://planet-sl.org/bgf" xmlns:xhtml="http://www.w3.org/1999/xhtml" version="1.0">
	<!-- wiki: BGF2Rascal -->
	<xsl:output method="text" encoding="UTF-8" omit-xml-declaration="yes"/>
	<xsl:param name="grammarname"/>
	<xsl:param name="imports" default="''"/>
	<xsl:template match="/bgf:grammar">
		<xsl:text>@contributor{BGF2Rascal automated exporter - SLPS - http://github.com/grammarware/slps/wiki/BGF2Rascal}
module </xsl:text>
		<xsl:call-template name="capitalise">
			<xsl:with-param name="n" select="$grammarname"/>
		</xsl:call-template>
		<xsl:text>

extend lang::std::Whitespace;
</xsl:text>
		<xsl:if test="root">
			<xsl:text>import ParseTree;
import util::IDE;
import IO;
</xsl:text>
		</xsl:if>
		<xsl:if test="$imports">
			<xsl:text>import </xsl:text>
			<xsl:value-of select="$imports"/>
			<xsl:text>;

</xsl:text>
		</xsl:if>
		<xsl:text>
layout Standard = Whitespace* !&gt;&gt; [\u0009-\u000D \u0020 \u0085 \u00A0 \u1680 \u180E \u2000-\u200A \u2028 \u2029 \u202F \u205F \u3000];
</xsl:text>
		<xsl:apply-templates select="./bgf:*"/>
		<xsl:if test="root">
			<xsl:text>
public void main()
{
	registerLanguage("</xsl:text>
			<xsl:call-template name="capitalise">
				<xsl:with-param name="n" select="$grammarname"/>
			</xsl:call-template>
			<xsl:text>", "ext", </xsl:text>
			<xsl:value-of select="root[1]"/>
			<xsl:text>(str input, loc org) {return parse(#</xsl:text>
			<xsl:value-of select="root[1]"/>
			<xsl:text>, input, org);});
	println("Language registered.");
}
</xsl:text>
		</xsl:if>
	</xsl:template>
	<xsl:template match="bgf:production">
		<xsl:text>syntax </xsl:text>
		<xsl:apply-templates select="./nonterminal"/>
		<xsl:text>
        = </xsl:text>
		<xsl:if test="./label">
			<xsl:value-of select="./label"/>
			<xsl:text>: </xsl:text>
		</xsl:if>
		<xsl:choose>
			<xsl:when test="./bgf:expression/choice">
				<xsl:for-each select="./bgf:expression/choice/bgf:expression">
					<xsl:if test="position()!= 1">
						<xsl:text>
        | </xsl:text>
					</xsl:if>
					<xsl:call-template name="no-parenthesis">
						<xsl:with-param name="expr" select="."/>
					</xsl:call-template>
				</xsl:for-each>
				<xsl:text>
</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>
        </xsl:text>
				<xsl:call-template name="no-parenthesis">
					<xsl:with-param name="expr" select="./bgf:expression"/>
				</xsl:call-template>
				<xsl:text>
</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text> ;
</xsl:text>
	</xsl:template>
	<xsl:template match="bgf:expression">
		<xsl:apply-templates select="./*"/>
	</xsl:template>
	<xsl:template match="marked">
		<!-- do not exist in Rascal -->
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="./*"/>
		<xsl:text>)</xsl:text>
	</xsl:template>
	<xsl:template match="plus">
		<xsl:apply-templates select="./*"/>
		<xsl:text>+</xsl:text>
	</xsl:template>
	<xsl:template match="star">
		<xsl:apply-templates select="./*"/>
		<xsl:text>*</xsl:text>
	</xsl:template>
	<xsl:template match="optional">
		<!--  (N ("," N)*)? is treated as {N ","}* -->
		<xsl:choose>
			<xsl:when test="local-name(./bgf:expression/*[1]) = 'sequence'                         and ./bgf:expression/sequence/bgf:expression[1]/*[1] = ./bgf:expression/sequence/bgf:expression[2]/star/bgf:expression/sequence/bgf:expression[2]/*[1]">
				<xsl:text>{</xsl:text>
				<xsl:apply-templates select="./bgf:expression/sequence/bgf:expression[1]"/>
				<xsl:text> </xsl:text>
				<xsl:apply-templates select="./bgf:expression/sequence/bgf:expression[2]/star/bgf:expression/sequence/bgf:expression[1]"/>
				<xsl:text>}*</xsl:text>
			</xsl:when>
			<!-- TODO: N ("," N)*) is NOT treated as {N ","}+ -->
			<xsl:otherwise>
				<xsl:apply-templates select="./*"/>
				<xsl:text>?</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="terminal">
		<xsl:text>"</xsl:text>
		<xsl:call-template name="escape">
			<xsl:with-param name="t" select="."/>
		</xsl:call-template>
		<xsl:text>"</xsl:text>
	</xsl:template>
	<xsl:template match="value">
		<xsl:choose>
			<xsl:when test=". = 'string'">
				<xsl:text>String</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>Integer</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="epsilon">
		<xsl:text>()</xsl:text>
	</xsl:template>
	<xsl:template match="empty">
		<xsl:text>()</xsl:text>
	</xsl:template>
	<xsl:template match="any">
		<xsl:text>ANY</xsl:text>
	</xsl:template>
	<xsl:template match="nonterminal">
		<xsl:call-template name="capitalise">
			<xsl:with-param name="n" select="."/>
		</xsl:call-template>
	</xsl:template>
	<xsl:template match="selectable">
		<xsl:choose>
			<xsl:when test="local-name(bgf:expression/*) = 'star'              or local-name(bgf:expression/*) = 'optional'              or local-name(bgf:expression/*) = 'plus'">
				<!-- <xsl:text>&lt;</xsl:text> -->
				<xsl:apply-templates select="bgf:expression"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="selector"/>
				<!-- <xsl:text>&gt;</xsl:text>-->
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates select="bgf:expression"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="selector"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template match="sequence">
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="./bgf:expression[1]/*"/>
		<xsl:for-each select="./bgf:expression[position()&gt;1]">
			<xsl:text> </xsl:text>
			<xsl:apply-templates select="./*"/>
		</xsl:for-each>
		<xsl:text>)</xsl:text>
	</xsl:template>
	<!-- inner choices - BNF bar -->
	<xsl:template match="choice">
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="./bgf:expression[1]/*"/>
		<xsl:for-each select="./bgf:expression[position()&gt;1]">
			<xsl:text> | </xsl:text>
			<xsl:apply-templates select="./*"/>
		</xsl:for-each>
		<xsl:text>)</xsl:text>
	</xsl:template>
	<xsl:template name="no-parenthesis">
		<xsl:param name="expr"/>
		<xsl:choose>
			<xsl:when test="$expr/selectable">
				<xsl:value-of select="$expr/selectable/selector"/>
				<xsl:text>: </xsl:text>
				<xsl:call-template name="no-parenthesis">
					<xsl:with-param name="expr" select="$expr/selectable/bgf:expression"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:when test="$expr/sequence">
				<xsl:apply-templates select="$expr/sequence/bgf:expression[1]/*"/>
				<xsl:for-each select="$expr/sequence/bgf:expression[position()&gt;1]">
					<xsl:text> </xsl:text>
					<xsl:apply-templates select="./*"/>
				</xsl:for-each>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates select="$expr"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template name="capitalise">
		<xsl:param name="n"/>
		<xsl:value-of select="translate(substring($n,1,1),'abcdefghijklmnopqrstuvwxyz-.','ABCDEFGHIJKLMNOPQRSTUVWXYZ__')"/>
		<xsl:value-of select="translate(substring($n,2),'-.','__')"/>
	</xsl:template>
	<xsl:template name="escape">
		<xsl:param name="t"/>
		<xsl:if test="$t != ''">
			<xsl:variable name="c" select="substring($t, 1, 1)"/>
			<xsl:if test="$c = '&lt;' or $c = '&gt;' or $c = '&quot;' or $c = '\' ">
				<xsl:text>\</xsl:text>
			</xsl:if>
			<xsl:value-of select="$c"/>
			<xsl:call-template name="escape">
				<xsl:with-param name="t" select="substring($t, 2)"/>
			</xsl:call-template>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet>
