#' =============================================================
#' @description ui output of Tutorial
#' @note
#'==============================================================

output$Tutorial_ui <- renderUI({
  fluidPage(
    style = "background-color:#F0FFFF;",
    HTML('<center><img src=\'./img/CMTCN_Tu.png\' 
           width="200" height="200"></center>'),
    includeMarkdown("./Instructions/landing.md")
  )
})