#' =============================================================
#' @description ui output of News
#' @note using markdown to describe the development news of SCANNER
#'==============================================================

output$News_ui <- renderUI({
  fluidPage(
    style = "background-color:#F0FFFF;",
    HTML('<center><img src=\'./img/CMTCN_News.png\' 
           width="180" height="180"></center>'),
    p(HTML('<a href="http://s11.flagcounter.com/more/RyQj">
           <img src="https://s11.flagcounter.com/map/RyQj/size_s/txt_000000/border_CCCCCC/pageviews_1/viewers_0/flags_0/" 
           alt="Flag Counter" border="0"></a>'),
      align='center'),
    includeMarkdown("./Instructions/news.md")
  )
})


