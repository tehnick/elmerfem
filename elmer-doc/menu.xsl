<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  
  <xsl:output method="html"/>
  <xsl:template match="/">
    <html>
      <head>
        <title>
          Elmer
        </title>
        <link rel="stylesheet" href="elmer.css"/>
        <!--        <style src="elmer.css" type="text/css"/> -->

      </head>

      <body bgcolor="#FFFFFF">
        <div class="headline">
          <h1>Paikat</h1>
          <h2><xsl:value-of select="html/head/title"/></h2>
        </div>

        <div class="menu">
          <ul>
            <li><a href="find.py">Download</a></li>
            <li><a href="submit.py">Compiling</a></li>
            <li><a href="submit.py">Manuals</a></li>
            <li><a href="mapview.py">Screenshots</a></li>
            <li><a href="mapview.py">FAQ</a></li>
          </ul>
        </div>

        <div class="content">
          <xsl:copy-of select="html/body"/>
        </div>
      </body>

    </html>
  </xsl:template>
</xsl:stylesheet>



