#'==============================================================
#'@description Server page for SCANNER
#'==============================================================
# load("./Profile/Project.Rd")
#'==============================================================
#'set max request size
#'source for help
#'print version of R
#'==============================================================
options(shiny.maxRequestSize = 100*1024^2)
source("helpers.R")
# print(sessionInfo())
#'==============================================================
#' Server functions are divided by tab
#' Getting start page's Server
#'==============================================================
shinyServer(function(input,output,session){
 # source("server-tab-Home.R",local = TRUE)
  source("server-tab-Start.R",local = TRUE)
  source("server-tab-Network.R",local = TRUE)
  #source("server-tab-Download.R",local = TRUE)
  source("server-tab-Tutorial.R",local = TRUE)
  source("server-tab-News.R",local = TRUE)
})
