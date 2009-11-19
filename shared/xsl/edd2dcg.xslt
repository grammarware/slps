<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:edd="http://planet-sl.org/edd">

  <xsl:output
      method="text"
      encoding="UTF-8"
      omit-xml-declaration="yes"
      />
  
  <xsl:template match="/edd:dialect">
    <xsl:text>grammar(g(Ps)) --> productions(Ps).
productions([H|T]) --> production(H), productions(T).
productions([A]) --> production(A).
</xsl:text>
    <xsl:choose>
      <xsl:when test="production/reverse-order">
        <xsl:text>production(p(NT,As)) --> alternatives(As), definingsymbol, name(NT), terminatorsymbol.</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>production(p(NT,As)) --> name(NT), definingsymbol, alternatives(As).</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>
symbols([H|T]) --> symbol(H), string(" "), symbols(T).
symbols([S]) --> symbol(S).
symbol(nt(N)) --> name(N).
</xsl:text>
    <xsl:if test="production/separator">
      <!-- separator symbol -->
      <xsl:call-template name="symbol2definition">
        <xsl:with-param name="predicate" select="'separatorsymbol'"/>
        <xsl:with-param name="symbol" select="production/separator"/>
      </xsl:call-template>
      <xsl:text>alternatives([A|T]) --> symbols(A), separatorsymbol, alternatives(T).</xsl:text>
    </xsl:if>
    <xsl:text>
alternatives([S]) --> symbols(S), terminatorsymbol.
</xsl:text>
    <!-- defining symbol -->
    <xsl:call-template name="symbol2definition">
      <xsl:with-param name="predicate" select="'definingsymbol'"/>
      <xsl:with-param name="symbol" select="production/defining"/>
    </xsl:call-template>
    <!-- terminator symbol -->
    <xsl:call-template name="symbol2definition">
      <xsl:with-param name="predicate" select="'terminatorsymbol'"/>
      <xsl:with-param name="symbol" select="production/terminator"/>
    </xsl:call-template>
    <!-- grouping symbols -->
    <xsl:if test="fragment/group">
      <xsl:call-template name="symbol2definition">
        <xsl:with-param name="predicate" select="'groupstart'"/>
        <xsl:with-param name="symbol" select="fragment/group/start"/>
      </xsl:call-template>
      <xsl:call-template name="symbol2definition">
        <xsl:with-param name="predicate" select="'groupend'"/>
        <xsl:with-param name="symbol" select="fragment/group/end"/>
      </xsl:call-template>
      <xsl:text>
symbol(br(S)) --> groupstart, symbolchoices(S), groupend.
symbolchoices([H|T]) --> symbols(H), choicesymbol, symbolchoices(T).
symbolchoices([S]) --> symbols(S).
</xsl:text>
    </xsl:if>
    <xsl:if test="fragment/choice-symbol">
      <xsl:call-template name="symbol2definition">
        <xsl:with-param name="predicate" select="'choicesymbol'"/>
        <xsl:with-param name="symbol" select="fragment/choice-symbol"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="optionality/symbol">
      <xsl:call-template name="symbol2definition">
        <xsl:with-param name="predicate" select="'optionalitysymbol'"/>
        <xsl:with-param name="symbol" select="optionality/symbol"/>
      </xsl:call-template>
      <xsl:text>symbol(opt(nt(N))) --> name(N), optionalitysymbol.
symbol(opt(br(S))) --> groupstart, symbolchoices(S), groupend, optionalitysymbol.
</xsl:text>
    </xsl:if>
    <!-- not implemented: optionality pair -->
    <!-- repetitions -->
    <xsl:text>
symbol(r(R,nt(N))) --> name(N), repetition(R).
symbol(r(R,br(S))) --> groupstart, symbolchoices(S), groupend, repetition(R).
</xsl:text>
    <xsl:for-each select="repetition">
      <xsl:text>repetition(</xsl:text>
      <xsl:value-of select="name"/>
      <xsl:text>) --> </xsl:text>
      <xsl:call-template name="transformSymbol">
        <xsl:with-param name="symbol" select="markup/symbol"/>
      </xsl:call-template>
      <xsl:text>.
</xsl:text>
    </xsl:for-each>
    <!-- terminals -->
    <xsl:if test="terminal">
	<!-- double quote (34) hard coded; single quote is 39 -->
	<xsl:text>symbol(t(Y)) --> terminalstart, string(V), {\+ member(34, V), string_to_list(Y,V)}, terminalend.
terminalstart --> [34].
terminalend --> [34].
</xsl:text>
    </xsl:if>
    <!-- everything else -->
    <xsl:text>
<![CDATA[newline --> [10].
layout --> [0' ], layout.
layout --> [0'	], layout.
layout --> [].
keyword(X) --> layout, string(X).
string([],X,X).
string([H|T1],[H|T2],X) :- string(T1,T2,X).
name(V) --> layout, letter(H), letters(T), { atom_chars(V,[H|T]) }.
letters([H|T]) --> letter(H), letters(T).
letters([]) --> [].
letter(H,[H|T],T) :- H >= 0'a, H =< 0'z.
letter(H,[H|T],T) :- H >= 0'A, H =< 0'Z.
letter(0'-,[0'-|T],T).

% Pretty-printing a flat-token-grammar
writegrammar(g(Ps)) :-
 write('<?xml version="1.0" encoding="UTF-8"?>'),nl,
 write('<rgf:grammar xmlns:rgf="http://planet-sl.org/rgf">'), nl,
 writeproductions(Ps),
 write('</rgf:grammar>').
writeproductions([H|T]) :- writeproduction(H), writeproductions(T).
writeproductions([]).
writeproduction(p(NT,As)) :-
 write('<entry>'), nl,
 write('<key>'),
 write(NT),
 write('</key>'), nl,
 writealternatives(As),
 write('</entry>'), nl.
writesymbols([H|T]) :- writesymbol(H), writesymbols(T).
writesymbols([]).
writesymbol(nt(N)) :- writent(N).
writesymbol(t(V)) :- writet(V).
writealternatives([A|T]) :-
 write('<rhs>'), nl,
 writesymbols(A),
 write('</rhs>'), nl,
 writealternatives(T).
writealternatives([]).
writegroupstart :- write('<meta-group-start/>'), nl.
writegroupend :- write('<meta-group-end/>'), nl.
writesymbol(br(S)) :- writegroupstart, writesymbolchoices(S), writegroupend.
writesymbolchoices([H1,H2|T]) :- writesymbols(H1), writechoicesymbol, writesymbolchoices([H2|T]).
writesymbolchoices([S]) :- writesymbols(S).
writechoicesymbol :- write('<meta-choice/>'), nl.
writeoptionalitysymbol :- write('<meta-optionality/>'), nl.
writesymbol(opt(nt(N))) :- writent(N), writeoptionalitysymbol.
writesymbol(opt(br(S))) :- writegroupstart, writesymbolchoices(S), writegroupend, writeoptionalitysymbol.
writesymbol(r(R,nt(N))) :- writent(N), writerepetition(R).
writesymbol(r(R,br(S))) :- writegroupstart, writesymbolchoices(S), writegroupend, writerepetition(R).
writerepetition(R) :- write('<meta-repetition>'), write(R), write('</meta-repetition>'), nl.
writenewline :- nl.
writestring(X) :- write('<text>'), write(X), write('</text>'), nl.
writent(X) :- write('<nonterminal>'), write(X), write('</nonterminal>'), nl.
writet(X) :- write('<terminal>'), write(X), write('</terminal>'), nl.

% The main wrapper thing
main(File) :- parseFile(File,grammar,S), writegrammar(S).

parseFile(File,P,R) :-
 open(File,read,Stream,[]),
 read_stream_to_codes(Stream, Contents),
 close(Stream),
 apply(P,[R,Contents,Rest]),
 eof(Rest,_).

eof([],[]).
eof([0' |T],R) :- eof(T,R).
eof([10|T],R) :- eof(T,R).]]>
</xsl:text>
  </xsl:template>

  <xsl:template name="transformSymbol">
    <xsl:param name="symbol"/>
    <xsl:choose>
      <xsl:when test="count($symbol/*) = 1">
        <xsl:call-template name="transformSymbolBit">
          <xsl:with-param name="bit" select="$symbol/*"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="transformSymbolBit">
          <xsl:with-param name="bit" select="$symbol/*[1]"/>
        </xsl:call-template>
        <xsl:for-each select="$symbol/*[position()>1]">
          <xsl:text>, </xsl:text>
          <xsl:call-template name="transformSymbolBit">
            <xsl:with-param name="bit" select="."/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="transformSymbolBit">
    <xsl:param name="bit"/>
    <xsl:choose>
      <xsl:when test="local-name($bit) = 'text'">
        <xsl:text>keyword("</xsl:text>
        <xsl:value-of select="$bit/text()"/>
        <xsl:text>")</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'choice'">
        <xsl:text>(</xsl:text>
        <xsl:for-each select="$bit/*">
          <xsl:call-template name="transformSymbolBit">
            <xsl:with-param name="bit" select="."/>
          </xsl:call-template>
          <xsl:text>; </xsl:text>
        </xsl:for-each>
        <xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'optional'">
        <xsl:text>(</xsl:text>
          <xsl:call-template name="transformSymbol">
            <xsl:with-param name="symbol" select="$bit"/>
          </xsl:call-template>
        <xsl:text>; epsilon)</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'newline'">
        <xsl:text>newline</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'less-indentation'">
        <xsl:text>lessindentation</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'same-indentation'">
        <xsl:text>sameindentation</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'more-indentation'">
        <xsl:text>moreindentation</xsl:text>
      </xsl:when>
      <xsl:when test="local-name($bit) = 'eight-spaces'">
        <xsl:text>string("        ")</xsl:text>
      </xsl:when>
      <!-- not implemented: counter, layout, font, anything -->
    </xsl:choose>
  </xsl:template>

  <xsl:template name="symbol2definition">
    <xsl:param name="predicate"/>
    <xsl:param name="symbol"/>
    <xsl:value-of select="$predicate"/>
    <xsl:text> --> </xsl:text>
    <xsl:call-template name="transformSymbol">
      <xsl:with-param name="symbol" select="$symbol"/>
    </xsl:call-template>
    <xsl:text>.
</xsl:text>
  </xsl:template>
</xsl:stylesheet>
