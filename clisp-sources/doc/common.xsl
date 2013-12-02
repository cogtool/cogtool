<?xml version="1.0"?>
<!DOCTYPE xsl:stylesheet [
          <!ENTITY lowercase "'abcdefghijklmnopqrstuvwxyz'">
          <!ENTITY uppercase "'ABCDEFGHIJKLMNOPQRSTUVWXYZ'">
]>

<!-- common settings for CLISP Implementation Notes formatting -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0" xmlns="http://www.w3.org/1999/xhtml">

<xsl:param name="html.stylesheet" select="'impnotes.css'"/>
<xsl:param name="link.mailto.url" select="'mailto:clisp-list@lists.sourceforge.net'"/>
<!-- xsl:param name="suppress.navigation" select="0"/-->
<xsl:param name="inherit.keywords" select="0"/>
<xsl:param name="variablelist.term.break.after" select="1"/>
<xsl:param name="variablelist.term.separator" select="''"/>

<!-- xsl:template name="user.header.content">
 <p>CLISP Implementation Notes [user.header.content]</p><hr width="100%"/>
</xsl:template -->

<!-- http://article.gmane.org/gmane.text.docbook.apps:9779 -->
<xsl:preserve-space elements="entry"/>

<!-- http://article.gmane.org/gmane.text.docbook.apps:11014
     apply-templates is mapc on children
     apply-imports is call-next-method -->

<xsl:template match="ulink[@url='google']">
 <a class="{@role}" href="http://www.google.com/search?q={.}"
    ><xsl:apply-templates/></a></xsl:template>

<!-- =============================== RFC =============================== -->
<xsl:param name="rfc.top" select="'http://rfc.net/rfc'"/>
<xsl:template match="ulink[@role='rfc']">
 <a class="{@role}" href="{$rfc.top}{@url}.html"><code>
   <xsl:choose><xsl:when test=".=''"><xsl:text>RFC</xsl:text>
     <xsl:value-of select="@url"/></xsl:when>
    <xsl:otherwise><xsl:apply-templates/></xsl:otherwise></xsl:choose>
 </code></a>
</xsl:template>
<!-- ============================== / RFC ============================== -->

<!-- ============================ CLISP CVS ============================ -->
<xsl:param name="clisp.cvs.top" select="'http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/*checkout*/clisp/clisp/'"/>
<xsl:template match="ulink[@role='clisp-cvs']">
 <a class="{@role}" href="{$clisp.cvs.top}{@url}"><xsl:apply-templates/></a>
</xsl:template>
<xsl:template match="filename[@role='clisp-cvs']">
 <a class="{@role}" href="{$clisp.cvs.top}{.}"><xsl:apply-imports/></a>
</xsl:template>
<!-- =========================== / CLISP CVS =========================== -->

<!-- =========== The Open Group Base Specifications Issue 6 ============ -->
<xsl:param name="unix.top"
           select="'http://www.opengroup.org/onlinepubs/007904975/'"/>
<xsl:template match="function[@role='unix'] | varname[@role='unix']">
 <a class="{@role}" href="{$unix.top}functions/{.}.html"
    ><xsl:apply-imports/></a>
</xsl:template>

<xsl:template match="command[@role='unix']">
 <a class="{@role}" href="{$unix.top}utilities/{substring-before(concat(normalize-space(.),' '),' ')}.html"><xsl:apply-imports/></a>
</xsl:template>

<xsl:template match="ulink[@role='unix']">
 <a class="{@role}" href="{$unix.top}{@url}"><xsl:apply-templates/></a>
</xsl:template>

<xsl:template match="filename[@role='unix']">
 <a class="{@role}" href="{$unix.top}basedefs/{.}.html"
    >&lt;<xsl:apply-imports/>&gt;</a> <!-- formatting for &lt;/&gt;? -->
 <!-- xsl:call-template name="filename">&lt;<xsl:value-of select="."/>&gt;</xsl:call-template -->
</xsl:template>
<!-- ========== / The Open Group Base Specifications Issue 6 =========== -->

<!-- =========================== Berkeley DB =========================== -->
<xsl:param name="bdb.top" select="'http://www.sleepycat.com/docs/'"/>
<xsl:template match="ulink[@role='bdb']">
 <a class="{@role}" href="{$bdb.top}{@url}"><xsl:apply-templates/></a>
</xsl:template>

<xsl:template match="function[@role='bdb']">
 <a class="{@role}"><xsl:attribute name="href">
   <xsl:value-of select="$bdb.top"/><xsl:text>api_c/</xsl:text>
   <xsl:choose>
    <xsl:when test=".='db_create'"><xsl:text>db_class</xsl:text></xsl:when>
    <xsl:when test=".='db_env_create'"><xsl:text>env_class</xsl:text></xsl:when>
    <xsl:when test=".='db_sequence_create'">
     <xsl:text>seq_class</xsl:text></xsl:when>
    <xsl:when test=".='db_strerror'">
     <xsl:text>env_strerror</xsl:text></xsl:when>
    <xsl:when test=".='db_version'"><xsl:text>env_version</xsl:text></xsl:when>
    <xsl:when test=".='log_compare'"><xsl:text>log_compare</xsl:text></xsl:when>
    <xsl:when test="starts-with(.,'DB_LOGC-')">
     <xsl:text>logc_</xsl:text></xsl:when>
    <xsl:when test="starts-with(.,'DB_TXN-')">
     <xsl:text>txn_</xsl:text></xsl:when>
    <xsl:when test="starts-with(.,'DB_ENV-')">
     <xsl:text>env_</xsl:text></xsl:when>
    <xsl:when test="starts-with(.,'DB_SEQUENCE-')">
     <xsl:text>seq_</xsl:text></xsl:when>
    <xsl:when test="starts-with(.,'DB_MPOOLFILE-')">
     <xsl:text>memp_</xsl:text></xsl:when>
    <xsl:when test="starts-with(.,'DBCursor-')">
     <xsl:text>dbc_</xsl:text></xsl:when>
    <xsl:when test="starts-with(.,'DB-')">
     <xsl:text>db_</xsl:text></xsl:when>
    <xsl:otherwise><xsl:message>unknown function/db element <xsl:value-of select="."/></xsl:message></xsl:otherwise>
   </xsl:choose>
   <xsl:value-of select="substring-after(.,'>')"/>
   <xsl:text>.html</xsl:text>
  </xsl:attribute>
  <xsl:apply-imports/>
</a></xsl:template>
<!-- ========================== / Berkeley DB ========================== -->

<!-- ========================== Matlab C API ========================== -->
<xsl:param name="matlab.top" select="'http://www.mathworks.com/access/helpdesk/help/techdoc/apiref/'"/>
<xsl:template match="function[@role='matlab'] | varname[@role='matlab']">
 <a class="{@role}" href="{$matlab.top}{translate(.,&uppercase;,&lowercase;)}.html"><xsl:apply-imports/></a>
</xsl:template>
<xsl:template match="ulink[@role='matlab']">
 <a class="{@role}" href="{$matlab.top}{@url}"><xsl:apply-templates/></a>
</xsl:template>
<!-- ========================= / Matlab C API ========================= -->

<!-- ========================== Netica C API ========================== -->
<xsl:param name="netica.top"
           select="'http://norsys.com/onLineAPIManual/functions/'"/>
<xsl:template match="function[@role='netica'] | varname[@role='netica']">
 <a class="{@role}" href="{$netica.top}{.}.html"><xsl:apply-imports/></a>
</xsl:template>
<!-- ========================= / Netica C API ========================= -->

<xsl:template match="literal[@role = 'type'
      or @role = 'method' or @role = 'data' or @role = 'byte']">
 <span class="{@role}"><xsl:apply-imports/></span>
</xsl:template>

<xsl:template match="emphasis[@role = 'plat-dep']">
 <span class="{@role}">
  <xsl:text>Platform Dependent: </xsl:text>
  <xsl:apply-imports/>
 </span>
</xsl:template>

<xsl:template match="isbn" mode="bibliography.mode">
 <xsl:text>ISBN&#160;</xsl:text>
 <xsl:apply-templates mode="bibliography.mode"/>
 <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="quote[@role = 'package']">
 <strong class="{@role}"><xsl:apply-imports/></strong>
</xsl:template>

<xsl:template match="firstterm">
 <strong class="first"><xsl:apply-imports/></strong>
</xsl:template>

<xsl:param name="generate.toc"> <!-- toc in refentry -->
refentry  toc
appendix  toc,title
article/appendix  nop
article   toc,title
book      toc,title,figure,table,example,equation
chapter   toc,title
part      toc,title
preface   toc,title
qandadiv  toc
qandaset  toc
reference toc,title
sect1     toc
sect2     toc
sect3     toc
sect4     toc
sect5     toc
section   toc
set       toc,title
</xsl:param>

<xsl:template match="programlisting/computeroutput">
 <xsl:text>&#8658;&#160;</xsl:text> <!-- &rArr; + &nbsp; -->
 <xsl:apply-imports/>
</xsl:template>

<xsl:template match="comment()">  <!-- pass through comments -->
 <xsl:text>&#10;</xsl:text>
 <xsl:comment><xsl:value-of select="normalize-space(.)"/></xsl:comment>
 <!-- http://article.gmane.org/gmane.text.docbook.apps:13033 -->
 <xsl:if test="not(following-sibling::node()[1][self::comment()])">
  <xsl:text>&#10;</xsl:text></xsl:if>
</xsl:template>

<xsl:param name="generate.section.toc.level" select="10"/>
<xsl:param name="toc.section.depth" select="10"/>
<xsl:param name="toc.max.depth" select="3"/>
<xsl:param name="generate.index" select="1"/>
<xsl:param name="refentry.generate.title" select="1"/>
<xsl:param name="use.id.as.filename" select="1"/>
<xsl:param name="section.autolabel" select="1"/>
<xsl:param name="section.label.includes.component.label" select="1"/>

<xsl:template name="user.footer.content">
 <xsl:if test="/refentry/refentryinfo"><div class="refentryinfo">
   <hr width="100%"/><table width="100%" summary="man page meta info">
    <th><td align="left">
      <xsl:apply-templates select="/refentry/refentryinfo/title/node()"/>
     </td><td align="center">
      <xsl:apply-templates select="/refentry/refentryinfo/subtitle/node()"/>
     </td><td align="right">
      <xsl:apply-templates select="/refentry/refentryinfo/date/node()"/>
 </td></th></table></div></xsl:if>
 <xsl:if test="/book/bookinfo"><div class="bookinfo">
   <hr width="100%"/><table width="100%" summary="impnotes meta info">
    <th><td align="left">
      <xsl:apply-templates select="/book/bookinfo/subtitle/node()"/>
     </td><td align="right">
      <xsl:apply-templates select="/book/bookinfo/date/node()"/>
 </td></th></table></div></xsl:if>
 <div class="custom-footer"><hr width="100%"/><table width="100%">
   <tr><td align="left"><a href="http://clisp.cons.org">
      <img src="clisp.png" width="48" height="48" alt="[CLISP home]"/></a></td>
    <td align="center"><a href="http://sourceforge.net/donate/index.php?group_id=1355"><img src="http://images.sourceforge.net/images/project-support.jpg" width="88" height="32" border="0" alt="[Support This Project]"/></a></td>
    <td align="right"><a href="http://sourceforge.net"><img width="125" height="37" alt="[SourceForge]" src="http://sflogo.sourceforge.net/sflogo.php?group_id=1355&amp;type=2&amp;page={@id}"/></a></td>
 </tr></table></div>
</xsl:template>

</xsl:stylesheet>
