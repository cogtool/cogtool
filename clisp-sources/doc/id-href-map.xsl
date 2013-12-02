<!-- CLISP Implementation Notes multi-piece ID to HREF map generator -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text" indent="no"/>
<xsl:template match="div|obj">
 <xsl:value-of select="@targetptr"/>
 <xsl:text>&#10;</xsl:text>
 <xsl:value-of select="@href"/>
 <xsl:text>&#10;</xsl:text>
 <xsl:apply-templates/>
</xsl:template>
<xsl:template match="ttl|xreftext"/>
<xsl:strip-space elements="*"/>
</xsl:stylesheet>
