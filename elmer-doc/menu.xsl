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
        <!--        <link rel="stylesheet" href="http://www.csc.fi/css/csc.css" type="text/css"/> -->

      </head>

      <body bgcolor="#FFFFFF">
        <div class="all">
          <div id="top">
            <h1>Elmer</h1>
            <!-- tools -->
            <div style="text-align: right;"><a href="@print_version@"><img style="border: 0px;" border="0" src="images/print_icon.gif"/></a></div>
          </div>

          <div id="middle">
            <!--          <h1><xsl:value-of select="/html/head/title"/></h1> -->
            <xsl:copy-of select="/html/body"/>

            <!-- copyright -->
            <div id="copyrights">
              <hr/>
              <script language="JavaScript1.2">
                <![CDATA[ 
                  <!--
                       var time=new Date();
                       var year=time.getYear();
                       document.write("<span class=\"copyright\">Copyright CSC " + year + ", ");
                       document.write("last modified:" + document.lastModified + "</span>");
                    // -->
                  ]]> 
                </script>
            </div>

          </div>


          <div id="left">
            <!--
Elmer
Overview
ElmerFront
ElmerSolver
ElmerPost
ElmerGrid
Examples
Papers
Download
Tutorials
Registration
News
> Contributors
Contact information

          -->
            <a href="front.html">Front page</a><br/>
            <a href="download.html">Download</a><br/>
            <a href="compilation.html">Compiling</a><br/>
            <a href="manuals.html">Documentation</a><br/>
            <a href="screenshots.html">Screenshots</a><br/>
            <a href="lists.html">Mailing lists</a><br/>
            <a href="faq.html">FAQ</a><br/>

        </div>

        <!--        <div id="right">
          <h3>News:</h3>
          <p>News items?</p>
        </div> -->

      </div>
      </body>


    </html>
  </xsl:template>
</xsl:stylesheet>



