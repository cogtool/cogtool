<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                version="1.0">
<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/fo/docbook.xsl"/>
<xsl:param name="paper.type" select="'letter'"/>
<xsl:param name="xep.extensions" select="1"/>
<xsl:param name="target.database.document" select="'olink-pile.xml'"/>
<xsl:param name="hyphenate" select="'false'"/>
<xsl:param name="alignment" select="'left'"/>
<xsl:param name="variablelist.as.blocks" select="1"/>
<xsl:param name="variablelist.term.break.after" select="1"/>
<xsl:param name="variablelist.term.separator" select="''"/>

<xsl:template match="epigraph">
 <fo:block text-align="right" margin-left="50%">
  <xsl:call-template name="anchor"/>
  <xsl:apply-templates select="para|simpara|formalpara|literallayout"/>
  <xsl:if test="attribution">
   <fo:inline>
    <xsl:text>--</xsl:text>
    <xsl:apply-templates select="attribution"/>
   </fo:inline>
  </xsl:if>
 </fo:block>
</xsl:template>

<xsl:template match="comment()">  <!-- pass through comments -->
 <xsl:text>&#10;</xsl:text>
 <xsl:comment><xsl:value-of select="normalize-space(.)"/></xsl:comment>
 <xsl:if test="not(following-sibling::comment())">
  <xsl:text>&#10;</xsl:text></xsl:if>
</xsl:template>

<xsl:template match="isbn" mode="bibliography.mode">
 <fo:inline>
  <xsl:text>ISBN&#160;</xsl:text>
  <xsl:apply-templates mode="bibliography.mode"/>
  <xsl:value-of select="$biblioentry.item.separator"/>
 </fo:inline>
</xsl:template>

<xsl:template match="emphasis[@role = 'plat-dep']">
 <fo:inline>
  <fo:inline font-weight="bold">Platform Dependent: </fo:inline>
  <xsl:apply-imports/>
 </fo:inline>
</xsl:template>

<xsl:param name="title.margin.left" select="'1pc'"/>
<!-- http://docbook.sourceforge.net/release/xsl/current/doc/fo/ulink.footnotes.html -->
<xsl:param name="ulink.footnotes" select="1"/>
<!-- http://docbook.sourceforge.net/release/xsl/current/doc/fo/ulink.show.html -->
<xsl:param name="ulink.show" select="0"/>

</xsl:stylesheet>
