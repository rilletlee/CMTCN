#' =============================================================
#' @description ui output of About
#'==============================================================
 
output$About_ui <- renderUI({
  fluidPage(
    style = "background-color:#F0FFFF;",
    HTML('<center><img src=\'./img/CMTCN_About.png\' 
           width="200" height="200"></center>'),
    hr(),
    column(
      width = 2,
      HTML('<p><p>
					 <a href="http://bioinforai.tech/wordpress/" target="_blank">
           <img id="logo" alt="Lab Logo" src="./img/Logo_Lab.png" width="250"/>
           </a>
           <p><p>')
    ),
    column(
      width = 10,
      HTML('
           CMTCN was built using 
           <a href="http://shiny.rstudio.com/", target="_blank"> Shiny </a> by 
           <a href="https://www.rstudio.com/", target="_blank"> RStudio, </a> using open source software.<br><br>
           We sincerely acknowledge all publicly available resources which have been used in CMTCN!<br><br>
           The following R packages are utilized in no particular order of importance:<br> 
            <a href="http://shiny.rstudio.com/"> Shiny </a>(>=0.14.2)<br>
            <a href="https://github.com/daattali/shinyjs">shinyjs</a>(>=0.8)<br>
            <a href="http://rstudio.github.io/shinydashboard/">shinydashboard</a>(>=0.7.0)<br>
            <a href="http://shiny.rstudio.com/articles/themes.html">shinythemes</a>(>=1.1.1)<br>
            <a href="http://cran.r-project.org/web/packages/data.table/">data.table</a>(>=1.10.4-3)<br>
            <a href="http://cran.r-project.org/package=DT">DT</a>(>=0.4)<br>
            <a href="http://cran.r-project.org/web/packages/RSQLite/index.html">RSQLite</a>(>=2.1.0)<br>
            <a href="https://cran.r-project.org/web/packages/DBI">DBI</a>(>=0.8)<br>
            <a href="http://cran.r-project.org/web/packages/ggplot2/index.html">ggplot2</a>(>=2.2.1)<br>
            <a href="http://cran.r-project.org/web/packages/ggthemes/index.html">ggthemes</a>(>=3.4.2)<br>
            <a href="http://cran.r-project.org/package=networkD3">networkD3</a>(>=0.4)<br>
            <a href="http://cran.r-project.org/package=dplyr">dplyr</a>(>=0.7.4)<br>
            <a href="https://rmarkdown.rstudio.com/">markdown</a>(>=0.8)<br>
            <a href="https://github.com/andrewsali/shinycssloaders">shinycssloaders</a>(>=0.2.0)<br>
            <a href="http://www.bioconductor.org/packages/release/bioc/html/clusterProfiler.html">clusterProfiler</a>(>=3.8.0)<br>
            <a href="http://www.bioconductor.org/packages/release/bioc/html/GSEABase.html">GSEABase</a>(>=1.42.0)<br>
            <a href="http://cran.r-project.org/web/packages/igraph/index.html">igraph</a>(>=1.2.1)<br>
            <a href="http://www.bioconductor.org/packages/release/bioc/html/RTCGAToolbox.html">RTCGAToolbox</a>(>=2.10.0)<br><br>
      '),
      HTML("TCGA expression data (run date 2016-01-28) provided in the <a href='http://gdac.broadinstitute.org/'>Firehose</a> 
           data repository are accessed using the R package RTCGAToolbox.<br><br>"),
      
      HTML("A list of resources which are utilized in CMTCN"),
      dataTableOutput("About_TBL")
      # HTML(markdown::markdownToHTML())
    )
  )
})
output$About_TBL <- DT::renderDataTable({
  Resource <- c(
                '<a href="http://genomeportal.stanford.edu/pan-tcga/">TCGA-CE</a>',
                '<a href="http://tcoa.cpu.edu.cn/index.php">The Cancer Omics Atlas</a>',
                '<a href="http://gdac.broadinstitute.org/">Firehose</a>',
                '<a href="http://itfp.biosino.org/itfp">ITFP</a>',
                '<a href="www.grnpedia.org/trrust">TRRUST v2</a>',
                '<a href="http://www.lbbc.ibb.unesp.br/htri">HTRIdb</a>',
                '<a href="http://rulai.cshl.edu/TRED">TRED</a>',
                '<a href="http://www.miR2Disease.org">miR2Disease</a>',
                '<a href="http://miRTarBase.mbc.nctu.edu.tw/">miRTarBase</a>',
                '<a href="http://c1.accurascience.com/miRecords/">miRecords</a>',
                '<a href="http://starbase.sysu.edu.cn/">starBase</a>',
                '<a href="http://www.targetscan.org/vert_70/">TargetScan</a>',
                '<a href="http://mcube.nju.edu.cn/jwang/lab/soft/mirtrans/">mirTrans</a>',
                '<a href="https://www.isical.ac.in/~bioinfo_miu/TF-miRNA1.php">PuTmiR</a>',
                '<a href="https://www.intogen.org/search">IntOGen</a>',
                '<a href="http://bioinfo.life.hust.edu.cn/SEGreg">SEGreg</a>',
                '<a href="http://zhaobioinfo.org/TissGDB ">TissGDB</a>',
                '<a href="http://www.cuilab.cn/hmdd">HMDD2.0</a>',
                '<a href="http://mircancer.ecu.edu/">miRCancer</a>',
                '<a href="http://www.miR2Disease.org">miR2Disease</a>',
                '<a href="http://mips.helmholtz-muenchen.de/phenomir">PhenomiR</a>'
  )
  #Confidence <- c("Experimental","Experimental","Experimental","Predicted","Experimental","Experimental","Experimental","Predicted","Predicted","Experimental","Predicted")
  Version <- c(
    "V2.0",
    "Aug.2018",
    "Jan.2016",
    "Aug.2008","V2.0","Aug.2012","Jan.2007","Apr.2008","V7.0","Apr.2013","V1.0","V7.0","Oct.2017","V1.1",
    "Dec.2014","Jan.2018","Sep.2017",
    "v2.0","Dec.2017","Apr.2008","Jan.2010"
  )
  Information  <- c(
                    "Clinical-stage related genes",
                    "Top mutated genes",
                    "TCGA expression data",
                    "TF->Gene interactions","TF->Gene interactions","TF->Gene interactions","TF->Gene interactions",
                    "miRNA->Gene/TF interactions","miRNA->Gene/TF interactions","miRNA->Gene/TF interactions","miRNA->Gene/TF interactions","miRNA->Gene/TF interactions",
                    "TF->miRNA interactions","TF->miRNA interactions",
                    "Cancer gene information", "Cancer gene information", "Cancer gene information",
                    "Cancer miRNA information","Cancer miRNA information","Cancer miRNA information","Cancer miRNA information")
   #nIter <- c(48861,8427,52467,7042,603,380639,1715,419578,100031,201364,10147)
  CMTCN_Res <-
    data.frame(
      Resource,
      # Confidence,
      #nIter,
      Version,
      Information
    )
  colnames(CMTCN_Res) <- c("Resource","Version/Release.Date","Information")
  DT::datatable(CMTCN_Res,
                selection = "none",
                filter = "none",
                escape = 1,
                options = list(pageLength = 10,dom = 'tip')
  )
})
 