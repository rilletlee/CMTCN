#' =============================================================
#' @description UI page for CMTCN
#'==============================================================
#'==============================================================
#' source for help
#'==============================================================
source("helpers.R")

#'==============================================================
#' navbarPage:always at the top of the page
#'==============================================================
navbarPage(

  # define the theme: using shinytheme
  theme = "cream.css",
  # fluid = T,
  # define the windows title
  windowTitle = HTML("CMTCN"),
  # define the Name of the web
  title = p(HTML("<img style='backfloat:left; margin-top:-10px;' 
                 src='./img/logo.png' 
                 width='50px' 
                 height='50px'/>"),
            HTML("<font size=\"4\" 
                 color=\"#00B2EE\">CMTCN
                 </font>")),
  # #record the Navy Id
  id = "NavyID",
  # source UI pages
  
 # source("ui-tab-Home.R",local = TRUE)$value,
  source("ui-tab-Start.R",local = TRUE)$value,
  source("ui-tab-Network.R",local = TRUE)$value,
 # source("ui-tab-Download.R",local = TRUE)$value,
  source("ui-tab-Tutorial.R",local = TRUE)$value,
  source("ui-tab-News.R",local = TRUE)$value,
  
  #'==============================================================
  #'Footer
  #'==============================================================
  footer = p(hr(),p("ShinyApp created by ",strong("Ruijiang LI (Rillet Lee)")," of Beijing Institute of Radiation Medicine",align="center",width=4),
           #p(("Code available on Github:"),a("https://github.com/",href=""),align="center",width=4),
           p("Feel free to contact Ruijiang LI (jqom@sohu.com) for any question or bug report.",align="center",width=4),
           p(("Copyright © 2018"),align="center",width=4)),
  tags$head(includeScript("google-analytics.js"))
)
#'==============================================================
#'End of the UI page
#'==============================================================