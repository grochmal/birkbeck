<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.w3.org/1999/xhtml">
  <!-- indent="yes" helps when debugging the stylesheet -->
  <xsl:output indent="yes"/>
  <xsl:template match="remakes">
    <html><body>
    <table class="results"><tbody>
      <tr>
        <td class="headrow">Title of remake</td>
        <td class="headrow">Year of remake</td>
        <td class="headrow">Title of original</td>
        <td class="headrow">Year of original</td>
        <td class="headrow">Faithfulness</td>
      </tr>
      <xsl:for-each select="remake">
        <xsl:sort select="rtitle"   order="ascending"  data-type="text"  />
        <!-- Try all permutations to check for bad data
        <xsl:sort select="rtitle"   order="ascending"  data-type="text"  />
        <xsl:sort select="ryear"    order="ascending"  data-type="number"/>
        <xsl:sort select="stitle"   order="ascending"  data-type="text"  />
        <xsl:sort select="syear"    order="ascending"  data-type="number"/>
        <xsl:sort select="fraction" order="ascending"  data-type="number"/>
        <xsl:sort select="rtitle"   order="descending" data-type="text"  />
        <xsl:sort select="ryear"    order="descending" data-type="number"/>
        <xsl:sort select="stitle"   order="descending" data-type="text"  />
        <xsl:sort select="syear"    order="descending" data-type="number"/>
        <xsl:sort select="fraction" order="descending" data-type="number"/>
        -->
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </tbody></table>
    </body></html>
  </xsl:template>
  <xsl:template match="remake">
    <tr class="tbrow">
      <td class="tbon" ><xsl:value-of select="rtitle"  /></td>
      <td class="tboff"><xsl:value-of select="ryear"   /></td>
      <td class="tbon" ><xsl:value-of select="stitle"  /></td>
      <td class="tboff"><xsl:value-of select="syear"   /></td>
      <td class="tbon" ><xsl:value-of select="fraction"/></td>
    </tr>
  </xsl:template>
</xsl:stylesheet>

