#' =============================================================
#' @description About page as a tab 
#'==============================================================
tabPanel(
  title = "About",icon = shiny::icon("glyphicon glyphicon-globe",lib = "glyphicon"),
  uiOutput("About_ui") %>% 
    withSpinner(
      color =  "#00AEAE",
      type = 5,
      size = 2
    ) 
)
#' =============================================================
#' End
#'==============================================================