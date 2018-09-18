#' =============================================================
#' @description Start page as a tab 
#'==============================================================
 
tabPanel(
  useShinyjs(),
  title = "Start",icon = icon("paper-plane",lib = "font-awesome"),
  dashboardPage(
    skin = "green",
    header = dashboardHeader(disable = T),
    sidebar = dashboardSidebar(disable = T),
    body =  uiOutput("Start_ui") 
  )  
) 


#' =============================================================
#' End
#'==============================================================