#' =============================================================
#' @description Download page as a tab 
#'==============================================================
tabPanel(
  title = "Download",icon = shiny::icon("glyphicon glyphicon-download-alt",lib = "glyphicon"),
  uiOutput("Download_ui") %>% 
    withSpinner(
      color =  "#32CD32",
      type = 1,
      size = 2
    )
)
#' =============================================================
#' End
#'==============================================================