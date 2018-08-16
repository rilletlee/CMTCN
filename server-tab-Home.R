#' =============================================================
#' @description ui output of Home_ui
#'==============================================================

output$Home_ui <- renderUI({
  # Add a row for the main picture
  fluidPage(
    useShinyjs(),
    style = "background-color:#F0FFFF;",
    br(),
    HTML('<center><img src=\'./img/SCANNER_Card.png\' 
         width="900" height="20"></center>'),
    br(),
    # Add an action button to redirect tab
    p(actionButton(inputId = "StartSC",
                   label = "Start",
                   width = "150px",
                   class = "btn-success",
                   icon = icon("star")),
      align="center"
    ),
    hr(),
    fluidRow(
      box(
        title = "Data Summary",
        width = 6,
        height = "auto",
        status = "info",
        # background = "olive",
        solidHeader = F,
        collapsible = F,
        collapsed = F,
        HTML('<center><img src=\'./img/Co-Reg_Num.png\' 
             width="600" height="1000"></center>')
        ),
      column(
        width = 6,
        fluidRow(
          box(
            title = "What is InCaCo",
            width = NULL,
            height = "auto",
            status = "info",
            solidHeader = F,
            collapsible = F,
            collapsed = F,
            textOutput("InCaCo_Des"),
            br()
          )  
        ),
        fluidRow(
          box(
            title = "Quick Start",
            width = NULL,
            height = "auto",
            status = "info",
            solidHeader = F,
            collapsible = F,
            collapsed = F,
            img(id="FG_imgACT",src="./img/InCaCo_QS.png",style="cursor:pointer;")
            # hr(),
            # img(id="SG_imgACT",src="/img/InCaCo_SG.png",style="cursor:pointer;")
            # HTML('<center><img src=\'./img/InCaCo_FG.png\' 
            #      width="600" height="300"></center>'),
            # hr(),
            # HTML('<center><img src=\'./img/InCaCo_SG.png\' 
            #      width="600" height="300"></center>')
            
            )  
          )
        )
      )
    )
}) 
output$InCaCo_Des <- renderText({
  "InCaCo provides manually curated microRNA (miRNA) and transcription factor (TF) co-regulatory relationships and comprehensive analysis within the context of different cancers. 
  For each type of cancer, InCaCo identifies four types of 3-nodes TF-miRNA co-regulatory motifs (three FFLs and one co-regulation motif) consisting of a TF, a miRNA and their co-targeted gene."                      
})
observeEvent(input$StartSC,{
  updateTabsetPanel(session,"NavyID",selected = "Browse")
})
observe({
  shinyjs::onclick("FG_imgACT",  updateTabsetPanel(session, inputId="NavyID", selected="Browse"))
})
# observe({
#   shinyjs::onclick("SG_imgACT", 
#                    updateRadioButtons(session,inputId = "NetType",selected = "Subgraph")
#   )
# })
