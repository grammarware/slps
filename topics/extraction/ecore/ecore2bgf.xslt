<xsl:stylesheet version = '1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
    xmlns:bgf="http://planet-sl.org/bgf"
    xmlns:xmi="http://www.omg.org/XMI"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:ecore="http://www.eclipse.org/emf/2002/Ecore">

  <xsl:output method="xml" encoding="UTF-8"/>

  <xsl:template match="/ecore:EPackage">
    <bgf:grammar>
      <xsl:apply-templates select="eClassifiers"/>
    </bgf:grammar>
  </xsl:template>

  <xsl:template match="/xmi:XMI">
    <bgf:grammar>
      <xsl:apply-templates select="ecore:EPackage/eClassifiers"/>
    </bgf:grammar>
  </xsl:template>

  <xsl:template match="eClassifiers">
    <xsl:variable name="ourEType" select="concat('#//',./@name)"/>
    <xsl:variable name="ourSuperType" select="substring(@eSuperTypes,4)"/>
    <xsl:choose>
      <xsl:when test="@abstract='true'">
        <bgf:production>
          <nonterminal>
            <xsl:value-of select="./@name"/>
          </nonterminal>
          <xsl:choose>
            <xsl:when test="//eClassifiers[@eSuperTypes=$ourEType]">
              <bgf:expression>
                <choice>
                  <xsl:for-each select="//eClassifiers[@eSuperTypes=$ourEType]">
                    <bgf:expression>
                      <nonterminal>
                        <xsl:value-of select="./@name" />
                      </nonterminal>
                    </bgf:expression>
                  </xsl:for-each>
                </choice>
              </bgf:expression>
            </xsl:when>
            <xsl:otherwise>
              <bgf:expression>
                <epsilon/>
              </bgf:expression>
            </xsl:otherwise>
          </xsl:choose>
        </bgf:production>
      </xsl:when>
      <xsl:when test="@name='DocumentRoot'"/>
      <xsl:when test="@xsi:type='ecore:EDataType'"/>
      <xsl:when test="@xsi:type='ecore:EClass'">
        <bgf:production>
          <nonterminal>
            <xsl:value-of select="./@name"/>
          </nonterminal>
          <xsl:choose>
            <xsl:when test="count(eStructuralFeatures)=0">
              <xsl:choose>
                <xsl:when test="//eClassifiers[@eSuperTypes=$ourEType]">
                  <bgf:expression>
                    <choice>
                      <xsl:for-each select="//eClassifiers[@eSuperTypes=$ourEType]">
                        <bgf:expression>
                          <nonterminal>
                            <xsl:value-of select="./@name" />
                          </nonterminal>
                        </bgf:expression>
                      </xsl:for-each>
                    </choice>
                  </bgf:expression>
                </xsl:when>
                <xsl:when test="//eClassifiers[@name=$ourSuperType and @abstract='true']">
                  <bgf:expression>
                    <sequence>
                      <xsl:apply-templates select="//eClassifiers[@name=$ourSuperType]/eStructuralFeatures"/>
                    </sequence>
                  </bgf:expression>
                </xsl:when>
                <xsl:otherwise>
                  <bgf:expression>
                    <epsilon/>
                  </bgf:expression>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
            <xsl:when test="count(eStructuralFeatures)=1">
              <xsl:apply-templates select="./eStructuralFeatures"/>
            </xsl:when>
            <xsl:otherwise>
              <bgf:expression>
                <sequence>
                  <xsl:apply-templates select="./eStructuralFeatures"/>
                </sequence>
              </bgf:expression>
            </xsl:otherwise>
          </xsl:choose>
        </bgf:production>
      </xsl:when>
      <xsl:when test="@xsi:type='ecore:EEnum'">
        <bgf:production>
          <nonterminal>
            <xsl:value-of select="./@name"/>
          </nonterminal>
          <xsl:choose>
            <xsl:when test="count(eLiterals)=0">
              <bgf:expression>
                <epsilon/>
              </bgf:expression>
            </xsl:when>
            <xsl:when test="count(eLiterals)=1">
              <xsl:apply-templates select="./eLiterals"/>
            </xsl:when>
            <xsl:otherwise>
              <bgf:expression>
                <choice>
                  <xsl:apply-templates select="./eLiterals"/>
                </choice>
              </bgf:expression>
            </xsl:otherwise>
          </xsl:choose>
        </bgf:production>
      </xsl:when>
      <xsl:otherwise>
        <any/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="eLiterals">
    <bgf:expression>
      <selectable>
        <selector>
          <xsl:value-of select="@name"/>
        </selector>
        <bgf:expression>
          <!-- something fishy
          <xsl:choose>
            <xsl:when test="@value">
              <terminal>
                <xsl:value-of select="@value"/>
              </terminal>
            </xsl:when>
            <xsl:otherwise>
              <epsilon/>
            </xsl:otherwise>
          </xsl:choose>
          -->
          <epsilon/>
        </bgf:expression>
      </selectable>
    </bgf:expression>
  </xsl:template>

  <xsl:template match="eStructuralFeatures">
    <xsl:choose>
      <xsl:when test="./@xsi:type='ecore:EReference'">
        <xsl:call-template name="mapEReference">
          <xsl:with-param name="ref" select="."/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="./@xsi:type='ecore:EClass'">
        <xsl:call-template name="mapEClass">
          <xsl:with-param name="class" select="."/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="./@xsi:type='ecore:EAttribute'">
        <xsl:call-template name="mapEAttribute">
          <xsl:with-param name="attr" select="."/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>!!!</xsl:text>
        <xsl:value-of select="./@xsi:type"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="mapEClass">
    <xsl:param name="class"/>
    <xsl:call-template name="mapQualifiedSymbol">
      <xsl:with-param name="name" select="$class/@name"/>
      <xsl:with-param name="lower" select="$class/@lowerBound"/>
      <xsl:with-param name="upper" select="$class/@upperBound"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="mapQualifiedSymbol">
    <xsl:param name="name"/>
    <xsl:param name="etype"/>
    <xsl:param name="lower" select="'no'"/>
    <xsl:param name="upper"/>
    <xsl:choose>
      <xsl:when test="$lower and $upper">
        <xsl:choose>
          <xsl:when test="$lower=0 and $upper=1">
            <bgf:expression>
              <optional>
                <xsl:call-template name="mapSymbol">
                  <xsl:with-param name="name" select="$name"/>
                  <xsl:with-param name="etype" select="$etype"/>
                </xsl:call-template>
              </optional>
            </bgf:expression>
          </xsl:when>
          <xsl:when test="$lower=0 and $upper=-1">
            <bgf:expression>
              <star>
                <xsl:call-template name="mapSymbol">
                  <xsl:with-param name="name" select="$name"/>
                  <xsl:with-param name="etype" select="$etype"/>
                </xsl:call-template>
              </star>
            </bgf:expression>
          </xsl:when>
          <xsl:when test="$lower=1 and $upper=-1">
            <bgf:expression>
              <plus>
                <xsl:call-template name="mapSymbol">
                  <xsl:with-param name="name" select="$name"/>
                  <xsl:with-param name="etype" select="$etype"/>
                </xsl:call-template>
              </plus>
            </bgf:expression>
          </xsl:when>
          <xsl:when test="$lower=1 and $upper=1">
            <xsl:call-template name="mapSymbol">
              <xsl:with-param name="name" select="$name"/>
              <xsl:with-param name="etype" select="$etype"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <!-- unknown bounds -->
            <xsl:call-template name="mapSymbol">
              <xsl:with-param name="name" select="$name"/>
              <xsl:with-param name="etype" select="$etype"/>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="$lower">
            <xsl:call-template name="mapQualifiedSymbol">
              <xsl:with-param name="name" select="$name"/>
              <xsl:with-param name="etype" select="$etype"/>
              <xsl:with-param name="lower" select="$lower"/>
              <xsl:with-param name="upper" select="1"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:when test="$upper">
            <xsl:call-template name="mapQualifiedSymbol">
              <xsl:with-param name="name" select="$name"/>
              <xsl:with-param name="etype" select="$etype"/>
              <xsl:with-param name="lower" select="1"/>
              <xsl:with-param name="upper" select="$upper"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="mapQualifiedSymbol">
              <xsl:with-param name="name" select="$name"/>
              <xsl:with-param name="etype" select="$etype"/>
              <xsl:with-param name="lower" select="1"/>
              <xsl:with-param name="upper" select="1"/>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="mapSymbol">
    <xsl:param name="name"/>
    <xsl:param name="etype"/>
    <xsl:choose>
      <xsl:when test="$etype">
        <bgf:expression>
          <selectable>
            <selector>
              <xsl:call-template name="mapName">
                <xsl:with-param name="name" select="$name"/>
              </xsl:call-template>
            </selector>
            <bgf:expression>
              <xsl:call-template name="mapEType">
                <xsl:with-param name="etype" select="$etype"/>
              </xsl:call-template>
            </bgf:expression>
          </selectable>
        </bgf:expression>
      </xsl:when>
      <xsl:otherwise>
        <bgf:expression>
          <nonterminal>
            <xsl:call-template name="mapName">
              <xsl:with-param name="name" select="$name"/>
            </xsl:call-template>
          </nonterminal>
        </bgf:expression>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="mapEReference">
    <xsl:param name="ref"/>
    <xsl:call-template name="mapQualifiedSymbol">
      <xsl:with-param name="name" select="$ref/@name"/>
      <xsl:with-param name="etype" select="$ref/@eType"/>
      <xsl:with-param name="lower" select="$ref/@lowerBound"/>
      <xsl:with-param name="upper" select="$ref/@upperBound"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="mapEAttribute">
    <xsl:param name="attr"/>
    <xsl:call-template name="mapQualifiedSymbol">
      <xsl:with-param name="name" select="$attr/@name"/>
      <xsl:with-param name="etype" select="$attr/@eType"/>
      <xsl:with-param name="lower" select="$attr/@lowerBound"/>
      <xsl:with-param name="upper" select="$attr/@upperBound"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="mapEType">
    <xsl:param name="etype"/>
    <xsl:choose>
      <xsl:when test="substring($etype,1,3)='#//'">
        <nonterminal>
          <xsl:value-of select="substring($etype,4)"/>
        </nonterminal>
      </xsl:when>
      <xsl:when test="$etype='ecore:EDataType http://www.eclipse.org/emf/2003/XMLType#//String'">
        <value>string</value>
      </xsl:when>
      <xsl:when test="$etype='ecore:EDataType http://www.eclipse.org/emf/2002/Ecore#//EString'">
        <value>string</value>
      </xsl:when>
      <xsl:when test="$etype='ecore:EDataType http://www.eclipse.org/emf/2003/XMLType#//Int'">
        <value>int</value>
      </xsl:when>
      <xsl:when test="$etype='ecore:EDataType http://www.eclipse.org/emf/2002/Ecore#//EInt'">
        <value>int</value>
      </xsl:when>
      <xsl:otherwise>
        <!-- unknown etype -->
        <any/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="mapName">
    <xsl:param name="name"/>
    <xsl:choose>
      <xsl:when test="substring($name,1,3)='#//'">
        <xsl:value-of select="substring($name,4)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>