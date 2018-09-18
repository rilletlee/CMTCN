#' =============================================================
#' @description Network page as a tab 
#'==============================================================
tabPanel(
  title = "Analysis",icon = icon("connectdevelop",lib = "font-awesome"),
  uiOutput("Network_ui") %>% 
    withSpinner(
      color =  "#00AEAE",
      type = 5,
      size = 2
    ) 
)
#' =============================================================
#' End
#'==============================================================