<!-- CLISP man page driver -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl"/>
<!-- xsl:import href="common.xsl"/ -->

<xsl:param name="man.string.subst.map">
 <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
 <!-- From Michael Smith <xmldoc@users.sourceforge.net> -->
 <!-- * This first set up substitutions attempts to "normalize" all -->
 <!-- * "converted" comments so that they are turned into real roff comments -->
 <!-- * (backward compatible ones with \" instead of \#) and are always -->
 <!-- * preceded and followed by a single newline in the output file -->
 <!-- * -->
 <!-- * roff-ify comment start and add newline before it -->
 <substitution oldstring="&#60;!--" newstring='&#10;.\"&#60;!--'/>
 <!-- * "squeeze" two newlines before comment start into single newline -->
 <substitution oldstring='&#10;&#10;.\"&#60;!--' newstring='&#10;.\"&#60;!--'/>
 <!-- * chomp any space after comment end -->
 <substitution oldstring='--&#62; ' newstring='--&#62;'/>
 <!-- * add newline after comment end -->
 <substitution oldstring='--&#62;' newstring='--&#62;&#10;'/>
 <!-- * "squeeze" two newlines after comment end into a single newline -->
 <substitution oldstring='--&#62;&#10;&#10;' newstring='--&#62;&#10;'/>
 <!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -->
 <substitution oldstring="\" newstring="\\"/>
 <substitution oldstring="\\fB" newstring="\fB"/>
 <substitution oldstring="\\fI" newstring="\fI"/>
 <substitution oldstring="\\fR" newstring="\fR"/>
 <substitution oldstring="\\%" newstring="\%"/>
 <substitution oldstring="\\&amp;" newstring="\&amp;"/>
 <substitution oldstring=".\\&#34;" newstring=".\&#34;"/>
 <substitution oldstring="-" newstring="\-"/>
 <substitution oldstring=".it 1 an\-trap" newstring=".it 1 an-trap"/>
 <substitution oldstring=".nr an\-no\-space\-flag 1" newstring=".nr an-no-space-flag 1"/>
 <substitution oldstring=".nr an\-break\-flag 1" newstring=".nr an-break-flag 1"/>
 <substitution oldstring="&#xA;'" newstring="&#xA;\'"/>
 <!-- * non-breaking space -->
 <substitution oldstring=" " newstring="\ "/>
 <!-- * left double quote -->
 <substitution oldstring="“" newstring="\(lq"/>
 <!-- * right double quote -->
 <substitution oldstring="”" newstring="\(rq"/>
 <!-- * left single quote -->
 <substitution oldstring="‘" newstring="\(oq"/>
 <!-- * right single quote -->
 <substitution oldstring="’" newstring="\(cq"/>
 <!-- * copyright sign -->
 <substitution oldstring="©" newstring="\(co"/>
 <!-- * registered sign -->
 <substitution oldstring="®" newstring="\(rg"/>
 <!-- * servicemark -->
 <substitution oldstring="℠" newstring="(SM)"/>
 <!-- * trademark -->
 <substitution oldstring="™" newstring="(TM)"/>
</xsl:param>

<!-- ==================================================================== -->

<!-- here we just convert comments into < ! - - ... - - >
  for further processing with sed(1) (see Makefile) -->
<xsl:template match="comment()">
 <xsl:text>&#60;!--</xsl:text> <!-- #\< ! - - -->
 <xsl:variable name="content">
  <xsl:call-template name="string.subst">
   <xsl:with-param name="string"><xsl:value-of select="."/></xsl:with-param>
   <xsl:with-param name="target" select="'&#10;'"/>
   <xsl:with-param name="replacement" select="'--&#62;&#10;&#60;!--'"/>
  </xsl:call-template>
 </xsl:variable>
 <xsl:value-of select="normalize-space($content)"/>
 <xsl:text>--&#62;</xsl:text> <!-- - - #\> -->
</xsl:template>

<!-- the following tries to preserve the comments
  it does not work because para|simpara|remark in list mode
  calls normalize-space() and removes the whitespace around comments
  <http://article.gmane.org/gmane.text.docbook.apps/6461> -->
<!--
<xsl:template match="comment()">
 <xsl:text>&#10;.&#92;&#34;</xsl:text> <!- - #\Newline . \ " - ->
 <xsl:variable name="content">
  <xsl:call-template name="string.subst">
   <xsl:with-param name="string"><xsl:value-of select="."/></xsl:with-param>
   <xsl:with-param name="target" select="'&#10;'"/>
   <xsl:with-param name="replacement" select="'&#10;.&#92;&#34;'"/>
  </xsl:call-template>
 </xsl:variable>
 <xsl:value-of select="normalize-space($content)"/>
 <xsl:if test="not(following-sibling::comment())">
  <xsl:text>&#10;</xsl:text></xsl:if>
</xsl:template>
-->

<!-- the following two templates make synonym options appear on
  separate lines (like we do in XHTML)
  it is not clear whether this is a good idea: ".TP" does not guarantee
  a new line, so the alternatives are:
       -h, -/-help
              Displays a help message on how to use clisp.
  and
       -h
       -/-help Displays a help message on how to use CLISP.
  it appears that the former is at least no worse than the latter,
  so we disable these templates -->
<!--
<xsl:template match="varlistentry/term|glossterm">
 <xsl:variable name="content"><xsl:apply-templates/></xsl:variable>
 <xsl:value-of select="normalize-space($content)"/>
 <xsl:text>&#10;.PD 0&#10;.TP&#10;</xsl:text>
</xsl:template>

<xsl:template
  match="varlistentry/term[position()=last()]|glossterm[position()=last()]"
  priority="2">
 <xsl:variable name="content"><xsl:apply-templates/></xsl:variable>
 <xsl:value-of select="normalize-space($content)"/>
</xsl:template>
-->

</xsl:stylesheet>
