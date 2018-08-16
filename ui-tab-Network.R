#' =============================================================
#' @description Network page as a tab 
#'==============================================================
tabPanel(
  title = "Analysis",icon = icon("connectdevelop",lib = "font-awesome"),
  uiOutput("Network_ui") %>% 
    withSpinner(
      color =  "#32CD32",
      type = 1,
      size = 2
    )
)
#' =============================================================
#' End
#'==============================================================