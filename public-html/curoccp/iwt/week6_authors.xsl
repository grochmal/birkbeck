<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.w3.org/1999/xhtml">
  <xsl:key name="author" match="//author/text()" use="."/>
  <xsl:template match="proceedings">
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <title><xsl:value-of select="name"/></title>
      </head>
      <body>
        <ul><xsl:for-each select="//author/text()[generate-id() = generate-id(key('author',.))]">
          <xsl:sort select="."/>
          <li><xsl:value-of select="."/></li>
        </xsl:for-each></ul>
        <xsl:apply-templates select="contents/article"/>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="article">
    <p>
      <xsl:apply-templates select="author"/>
      <br/>
      <em>"<xsl:value-of select="title"/>"</em>
      , pp. 
      <xsl:value-of select="pages"/>.
    </p>
  </xsl:template>
  <xsl:template match="author">
    <xsl:value-of select="."/>
    <xsl:choose>
      <xsl:when test="position()=last()"></xsl:when>
      <xsl:otherwise>, </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>

