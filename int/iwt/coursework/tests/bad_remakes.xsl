<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.w3.org/1999/xhtml">
  <!-- indent="yes" is not supported in firefox -->
  <xsl:output indent="yes"/>
  <xsl:template match="remakes">
    <remakes>
      <xsl:for-each select="remake">
        <xsl:sort select="rtitle" order="ascending" data-type="text"/>
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </remakes>
  </xsl:template>
  <xsl:template match="remake">
    <remake>
      <rtitle><xsl:value-of select="rtitle"/></rtitle>
      <ryear><xsl:value-of select="ryear"/></ryear>
      <stitle><xsl:value-of select="stitle"/></stitle>
      <syear><xsl:value-of select="syear"/></syear>
      <fraction><xsl:value-of select="fraction"/></fraction>
    </remake>
  </xsl:template>
</xsl:stylesheet>

