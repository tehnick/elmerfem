<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  
  <xsl:output method="html"/>
  <xsl:template match="/">
    <html>
      <head>
        <title>
          Elmer: <xsl:value-of select="html/head/title"/>
        </title>
        <link rel="stylesheet" href="elmer.css"/>

      </head>

      <body bgcolor="#FFFFFF">
        <div class="headline">
          <h1>Elmer</h1>
        </div>

        <div class="menu">
          <ul>
            <li><a href="download.html">Download</a></li>
            <li><a href="compilation.html">Compiling</a></li>
            <li><a href="manuals.html">Demos</a></li>
            <li><a href="manuals.html">Manuals</a></li>
            <li><a href="screenshots.html">Screenshots</a></li>
            <li><a href="lists.html">Mailing lists</a></li>
            <li><a href="faq.html">FAQ</a></li>
          </ul>
        </div>

        <div class="content">
          <h1><xsl:value-of select="html/head/title"/></h1>
          <xsl:copy-of select="html/body"/>
        </div>
      </body>

    </html>
  </xsl:template>
</xsl:stylesheet>



