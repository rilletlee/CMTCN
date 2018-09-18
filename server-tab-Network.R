#' =============================================================
#' @description source functions
#' =============================================================

source("./Functions/01PickCo-Data.R")
source("./Functions/02SwitchCo-Data.R")
source("./Functions/03DrawNetwork.R")
source("./Functions/04clusterProfiler.R")
source("./Functions/05Topology.R")

# load(file = "./Profile/Project.Rd")

#' =============================================================
#' @description Network_ui
#' @tab:Co-regulatory Interactions turn to interaction page
#' =============================================================

output$Network_ui <- renderUI({
  fluidPage(
    #shinyjs must be initialized with a call to useShinyjs() in the app’s ui
    useShinyjs(),
    style = "background-color:#F0FFFF;",
    br(),
 
    uiOutput("ControlPanel_UI"),
    tabBox(
      id = "Tab_Network",
      title = "Network Analysis",
      selected = "Network Visualization",
      side = "left",
      width = 40,
      height = "auto",
      tabPanel(
        title = "Network Visualization",
        uiOutput("Visnet_UI")%>% 
          withSpinner(
            color =  "#00AEAE",
            type = 5,
            size = 2
          )  
      ),
      #Network topology
      tabPanel("Network Topology",
               uiOutput("Topology_UI") %>% 
                 withSpinner(
                   color =  "#00AEAE",
                   type = 5,
                   size = 2
                 ) 
      ),
      tabPanel(
        title = "Co-regulatory Interactions",
        uiOutput("CRI_UI")  %>% 
          withSpinner(
            color =  "#00AEAE",
            type = 5,
            size = 2
          )   
      ),
      tabPanel("Gene Enrichment",
               uiOutput("GE_UI") %>% 
                 withSpinner(
                   color =  "#00AEAE",
                   type = 5,
                   size = 2
                 ) 
      ),
      #miRNA enrichment analysis and annotation
      tabPanel("miRNA Enrichment",
               uiOutput("miRE_UI") %>% 
                 withSpinner(
                   color =  "#00AEAE",
                   type = 5,
                   size = 2
                 ) 
      )
      
    )
  )
})

#' *******************************************************
#' @description controlPanel ui 
#' *******************************************************
output$ControlPanel_UI<- renderUI({
  fluidPage(
    fluidRow(
      box(
        title = "Co-regulatory Pairs",
        width = 3,
        height = "auto",
        status = "info",
        solidHeader = F,
        collapsible = F,
        collapsed = F,
        uiOutput("ControlSP_UI")
      ),
      box(
        title = "TF-FFLs",
        width = 3,
        height = "auto",
        status = "info",
        solidHeader = F,
        collapsible = F,
        collapsed = F,
        uiOutput("ControlTFFL_UI")
      ),
      box(
        title = "miRNA-FFLs",
        width = 3,
        height = "auto",
        status = "info",
        solidHeader = F,
        collapsible = F,
        collapsed = F,
        uiOutput("ControlMFFL_UI")
      ),
      box(
        title = "Composite-FFLs",
        width = 3,
        height = "auto",
        status = "info",
        solidHeader = F,
        collapsible = F,
        collapsed = F,
        uiOutput("ControlCFFL_UI")
      )
    )
  )
})

#' *******************************************************
#' @description significant control ui 
#' *******************************************************
output$ControlSP_UI <- renderUI({
  fluidPage(
    fluidRow(
      HTML('<center><img src=\'./img/pair.png\' 
           width="100" height="100"></center>')
    ),
    fluidRow(
       p("Num:",textOutput("SP_Num",inline = T))
    ),
    fluidRow(
      checkboxInput("SP_CK", label = "Display", value = TRUE)
    ),
   
    # Only show this panel if select display
    conditionalPanel(
      condition = "input.SP_CK",
      fluidRow(
        checkboxInput("SP_CT", label = "Cutoff", value = TRUE)
      ),
      conditionalPanel(
        condition = "input.SP_CT",
        sliderInput("SP_PC", 
                    label = "p-value Cutoff", 
                    min = 0, 
                    max = 0.05, 
                    value = 0.05,
                    step = 0.01),
        sliderInput("SP_RC", 
                    label = "Correlation Coefficient Cutoff", 
                    min = 0, 
                    max = 1, 
                    value = 0.2,
                    step = 0.05),
        radioButtons(inputId = "SP_TFR",
                     label = "TF Regulation",
                     choices = c(
                       "Positive" = "Pos",
                       "Negative" = "Neg",
                       "Both" = "Bot"
                     ),
                     selected = c("Bot"),
                     inline = T)
      )
   
    )
  )
})
output$SP_Num <- renderText({
  SP_Data <- Load_SP()
  validate(
    need(!is.null(SP_Data),
         message = "NULL")
  )
  SP_num <- nrow(SP_Data)
  return(SP_num)
})
#' *******************************************************
#' @description TF FFL ui 
#' *******************************************************
output$ControlTFFL_UI <- renderUI({
  fluidPage(
    fluidRow(
      HTML('<center><img src=\'./img/TF-FFL.png\' 
           width="100" height="100"></center>')
      ),
    fluidRow(
      p("Num:",textOutput("TFFL_Num",inline = T))
    ),
    fluidRow(
      checkboxInput("TFFL_CK", label = "Display", value = TRUE)
    ),
    
    # Only show this panel if select display
    conditionalPanel(
      condition = "input.TFFL_CK",
      fluidRow(
        checkboxInput("TFFL_CT", label = "Cutoff", value = TRUE)
      ),
      conditionalPanel(
        condition = "input.TFFL_CT",
        sliderInput("TFFL_PC", 
                    label = "p-value Cutoff", 
                    min = 0, 
                    max = 0.05, 
                    value = 0.05,
                    step = 0.01),
        sliderInput("TFFL_RC", 
                    label = "Correlation Coefficient Cutoff", 
                    min = 0, 
                    max = 1, 
                    value = 0.2,
                    step = 0.05),
        radioButtons(inputId = "TFFL_TFR",
                     label = "TF Regulation",
                     choices = c(
                       "Positive" = "Pos",
                       "Negative" = "Neg",
                       "Both" = "Bot"
                     ),
                     selected = c("Bot"),
                     inline = T)
      )
      
    )
  )
})
output$TFFL_Num <- renderText({
  TFFL_Data <- Load_TFFL()
  validate(
    need(!is.null(TFFL_Data),
         message = "NULL")
  )
  TFFL_num <- nrow(TFFL_Data)
  return(TFFL_num)
})
#' *******************************************************
#' @description miRNA FFL control
#' *******************************************************
output$ControlMFFL_UI <- renderUI({
  fluidPage(
    fluidRow(
      HTML('<center><img src=\'./img/miRNA-FFL.png\' 
           width="100" height="100"></center>')
    ),
    fluidRow(
      p("Num:",textOutput("MFFL_Num",inline = T))
    ),
    fluidRow(
      checkboxInput("MFFL_CK", label = "Display", value = TRUE)
    ),
    
    # Only show this panel if select display
    conditionalPanel(
      condition = "input.MFFL_CK",
      fluidRow(
        checkboxInput("MFFL_CT", label = "Cutoff", value = TRUE)
      ),
      conditionalPanel(
        condition = "input.MFFL_CT",
        sliderInput("MFFL_PC", 
                    label = "p-value Cutoff", 
                    min = 0, 
                    max = 0.05, 
                    value = 0.05,
                    step = 0.01),
        sliderInput("MFFL_RC", 
                    label = "Correlation Coefficient Cutoff", 
                    min = 0, 
                    max = 1, 
                    value = 0.2,
                    step = 0.05),
        radioButtons(inputId = "MFFL_TFR",
                     label = "TF Regulation",
                     choices = c(
                       "Positive" = "Pos",
                       "Negative" = "Neg",
                       "Both" = "Bot"
                     ),
                     selected = c("Bot"),
                     inline = T)
      )
      
    )
  )
})
output$MFFL_Num <- renderText({
  MFFL_Data <- Load_MFFL()
  validate(
    need(!is.null(MFFL_Data),
         message = "NULL")
  )
  MFFL_num <- nrow(MFFL_Data)
  return(MFFL_num)
})
#' *******************************************************
#' @description composite FFL control
#' *******************************************************
output$ControlCFFL_UI <- renderUI({
  fluidPage(
    fluidRow(
      HTML('<center><img src=\'./img/Composite-FFL.png\' 
           width="100" height="100"></center>')
      ),
    fluidRow(
      p("Num:",textOutput("CFFL_Num",inline = T))
    ),
    fluidRow(
      checkboxInput("CFFL_CK", label = "Display", value = TRUE)
    ),
    
    # Only show this panel if select display
    conditionalPanel(
      condition = "input.CFFL_CK",
      fluidRow(
        checkboxInput("CFFL_CT", label = "Cutoff", value = TRUE)
      ),
      conditionalPanel(
        condition = "input.CFFL_CT",
        sliderInput("CFFL_PC", 
                    label = "p-value Cutoff", 
                    min = 0, 
                    max = 0.05, 
                    value = 0.05,
                    step = 0.01),
        sliderInput("CFFL_RC", 
                    label = "Correlation Coefficient Cutoff", 
                    min = 0, 
                    max = 1, 
                    value = 0.2,
                    step = 0.05),
        radioButtons(inputId = "CFFL_TFR",
                     label = "TF Regulation",
                     choices = c(
                       "Positive" = "Pos",
                       "Negative" = "Neg",
                       "Both" = "Bot"
                     ),
                     selected = c("Bot"),
                     inline = T)
      )
      
    )
  )
})
output$CFFL_Num <- renderText({
  CFFL_Data <- Load_CFFL()
  validate(
    need(!is.null(CFFL_Data),
         message = "NULL")
  )
  CFFL_num <- nrow(CFFL_Data)
  return(CFFL_num)
})

#'********************TAB Network Topology**********
#' ===========================================================
#' @description Topology_UI the UI of network topology
#' ===========================================================

output$Topology_UI <- renderUI({
  fluidPage(
    fluidRow(
      p(
        "Number of Nodes:",
        textOutput("Net_nN",inline = T),
        "Number of Edges:",
        textOutput("Net_nE",inline = T),
        "Network Density:",
        textOutput("Net_DS",inline = T),
        actionButton("TopoDownload", "Download network information")
        # downloadButton(outputId = "TopoDownload",
        #                label = "Download network information",
        #                class = "btn-success"
        # )
      )
    ),
    fluidRow(
      box(
        title = "Nodes Degrees",
        width = NULL,
        height = "auto",
        status = "info",
        solidHeader = F,
        collapsible = F,
        collapsed = F,
        uiOutput("Degree_UI")
      )
    ),
    fluidRow(
      box(
        title = "Hubs and Authorities",
        width = NULL,
        height = "auto",
        status = "info",
        solidHeader = F,
        collapsible = F,
        collapsed = F,
        uiOutput("HA_UI")
      )
    )
  #'========================================================
  #'@description the test table 
  #'========================================================
  # fluidRow(
  #   box(
  #     title = "Test",
  #     width = NULL,
  #     height = "auto",
  #     status = "info",
  #     solidHeader = F,
  #     collapsible = F,
  #     collapsed = F,
  #     DT::dataTableOutput("topo_Test")
  #   )
  # )
  )
})
output$Net_nN <- renderText({
  net_obj <- net_object()
  nN <- net_NodeCount(net_obj)
  return(nN)
})
output$Net_nE <- renderText({
  net_obj <- net_object()
  nE <- net_EdgeCount(net_obj)
  return(nE)
})
output$Net_DS <- renderText({
  net_obj <- net_object()
  ed <- edge_density(net_obj)
  ed <- round(ed,5)
  return(ed)
})


#' ===========================================================
#' @description the node degrees ui
#' ===========================================================
output$Degree_UI <- renderUI({
  fluidPage(
    fluidRow(
      column(
        width = 3,
        DT::dataTableOutput("Degree_DT") %>% 
          withSpinner(
            color =  "#32CD32",
            type = 1,
            size = 2
          )  
      ),
      column(
        width = 4,
        br(),
        br(),
        br(),
        br(),
        plotOutput("Degree_Hist") %>% 
          withSpinner(
            color =  "#32CD32",
            type = 1,
            size = 2
          )  
      ),
      column(
        width = 5,
        br(),
        br(),
        br(),
        br(),
        plotOutput("Degree_Dist") %>% 
          withSpinner(
            color =  "#32CD32",
            type = 1,
            size = 2
          )  
      )
    )
  )
})
#nodes degrees datatable
#'****note: setting server=F, it will download full page
output$Degree_DT <- renderDataTable(server = F,{
  net_obj <- net_object()
  deg <- degree(net_obj, mode="all")
  validate(
    need(length(deg)!=0,
         message = "No data available!")
  )
  degree_dt <- data.frame(Nodes=names(deg),Degree=deg,
                          row.names = NULL,stringsAsFactors = F)
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite,"./Database/GTM.db")
  GTM <- dbReadTable(con,"GTM")
  dbDisconnect(con)
  degree_dt <- dplyr::left_join(degree_dt,GTM,by=c("Nodes"="name"))%>%
    dplyr::rename("Group"="group")
  degree_dt <- degree_dt[order(degree_dt$Degree,decreasing = T),]
  rownames(degree_dt) <- NULL
  DT::datatable(degree_dt, 
                extensions = c('Buttons'),
                selection = c('none'),
                options = list(
                  pageLength=10,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                  # scollerY = 50,
                  # scoller = T
                )
  )
})
#nodes degrees histgraph
output$Degree_Hist <- renderPlot({
  net_obj <- net_object()
  Degree <- degree(net_obj, mode="all")
  validate(
    need(length(Degree)!=0,
         message = "No data available!")
  )
  p=hist(Degree, breaks=1:vcount(net_obj)-1, main="Histogram of node degree")
  return(p)
})
#nodes distribution
output$Degree_Dist <- renderPlot({
  net_obj <- net_object()
  deg <- degree(net_obj, mode="all")
  validate(
    need(length(deg)!=0,
         message = "No data available!")
  )
  deg.dist <- degree_distribution(net_obj,cumulative=T, mode="all")
  p <- plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
        xlab="Degree", ylab="Cumulative frequency",main=" Degree distribution")
  return(p)
})
#' ===========================================================
#' @description the Hubs and authorities UI
#' ===========================================================
output$HA_UI <- renderUI({
  fluidPage(
    # helpText("
    # The hubs and authorities algorithm developed by Jon Kleinberg was initially used to examine web pages.
    # Hubs were expected to contain catalogs with a large number of outgoing links; 
    # while authorities would get many incoming links from hubs, presumably because of their high-quality relevant information.
    # "),
    fluidRow(
      HTML("<font size=\"4\" 
                 color=\"#00B2EE\">Hub scores
                 </font>"),
      br(),
      br(),
      column(
        width = 6,
        DT::dataTableOutput("Hub_DT") %>% 
          withSpinner(
            color =  "#32CD32",
            type = 1,
            size = 2
          )  
      ),
      column(
        width = 6,
        helpText("The hub score of nodes.Show up to 50 nodes at most."),
        plotOutput("Hub_Plot") %>% 
          withSpinner(
            color =  "#32CD32",
            type = 1,
            size = 2
          )  
      )
    ),
    hr(),
    fluidRow(
      HTML("<font size=\"4\" 
                 color=\"#00B2EE\">Authority scores
             </font>"),
      br(),
      br(),
      column(
        width = 6,
        DT::dataTableOutput("Aus_DT") %>% 
          withSpinner(
            color =  "#32CD32",
            type = 1,
            size = 2
          )  
      ),
      column(
        width = 6,
        helpText("The authority score of nodes.Show up to 50 nodes at most."),
        plotOutput("Aus_Plot") %>% 
          withSpinner(
            color =  "#32CD32",
            type = 1,
            size = 2
          )  
      )
    )
  )

})
#hub nodes datatable
output$Hub_DT <- renderDataTable(server = F,{
  net_obj <- net_object()
  hubscore <- hub_score(net_obj, weights=NA)$vector
  validate(
    need(length(hubscore)!=0,
         message = "No data available!")
  )
  hs_dt <- data.frame(Nodes=names(hubscore),Score = hubscore,
                          row.names = NULL,stringsAsFactors = F)
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite,"./Database/GTM.db")
  GTM <- dbReadTable(con,"GTM")
  dbDisconnect(con)
  hs_dt <- dplyr::left_join(hs_dt,GTM,by=c("Nodes"="name"))%>%
    dplyr::rename("Group"="group")
  hs_dt$Score <- round(hs_dt$Score,5)
  hs_dt <- hs_dt[order(hs_dt$Score,decreasing = T),]
  rownames(hs_dt) <- NULL
  DT::datatable(hs_dt, 
                extensions = 'Buttons', 
                selection = c("none"),
                options = list(
                  pageLength=10,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                )
  )
})

output$Aus_DT <- renderDataTable(server = F,{
  net_obj <- net_object()
  ausscore <- authority_score(net_obj, weights=NA)$vector
  validate(
    need(length(ausscore)!=0,
         message = "No data available!")
  )
  aus_dt <- data.frame(Nodes=names(ausscore),Score = ausscore,
                      row.names = NULL,stringsAsFactors = F)
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite,"./Database/GTM.db")
  GTM <- dbReadTable(con,"GTM")
  dbDisconnect(con)
  aus_dt <- dplyr::left_join(aus_dt,GTM,by=c("Nodes"="name"))%>%
    dplyr::rename("Group"="group")
  aus_dt$Score <- round(aus_dt$Score,5)
  aus_dt <- aus_dt[order(aus_dt$Score,decreasing = T),]
  rownames(aus_dt) <- NULL
  DT::datatable(aus_dt, 
                extensions = 'Buttons',
                selection = c("none"),
                options = list(
                  pageLength=10,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                )
  )
})

#hubs nodes plot
output$Hub_Plot <- renderPlot({
  net_obj <- net_object()
  hubscore <- hub_score(net_obj, weights=NA)$vector
  validate(
    need(length(hubscore)!=0,
         message = "No data available!")
  )
  hs_dt <- data.frame(Nodes=names(hubscore),Score = hubscore,
                      row.names = NULL,stringsAsFactors = F)
  hs_dt <- hs_dt[order(hs_dt$Score,decreasing = T),]
  rownames(hs_dt) <- NULL
  #select top 50
  if(nrow(hs_dt)<=50){
    hs_dt <- hs_dt
  }else{
    hs_dt <- hs_dt[1:50,]
  }
  ScoreData <- data.frame(x=hs_dt$Nodes,y=hs_dt$Score)
  # Horizontal 
  # note: shape = integer between 0 and 25
  # note: stroke exists only for shapes between 1 and 24
  # note: style can be in: "blank", "solid", "dashed", "dotted", "dotdash", "longdash","twodash"
  ScoreP <-
    ggplot(ScoreData, aes(x=x, y=y)) +
    geom_segment(aes(x=x, xend=x, y=0, yend=y), 
                 color="blue",
                 linetype="solid") +
    geom_point(color="red",
               size=4,
               fill=alpha("orange", 0.3),
               alpha=0.6,
               shape=20,
               stroke=2) +
    theme_light() +
    coord_flip() +
    theme(panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()) +
    xlab("Node")+
    ylab("Score")
  # main("Node Score")
  # ScoreP
  return(ScoreP)
})

output$Aus_Plot <- renderPlot({
  net_obj <- net_object()
  ausscore <- authority_score(net_obj, weights=NA)$vector
  validate(
    need(length(ausscore)!=0,
         message = "No data available!")
  )
  aus_dt <- data.frame(Nodes=names(ausscore),Score = ausscore,
                       row.names = NULL,stringsAsFactors = F)
  aus_dt <- aus_dt[order(aus_dt$Score,decreasing = T),]
  rownames(aus_dt) <- NULL
  #select top 50
  if(nrow(aus_dt)<=50){
    aus_dt <- aus_dt
  }else{
    aus_dt <- aus_dt[1:50,]
  }
  ScoreData <- data.frame(x=aus_dt$Nodes,y=aus_dt$Score)
  # Horizontal 
  # note: shape = integer between 0 and 25
  # note: stroke exists only for shapes between 1 and 24
  # note: style can be in: "blank", "solid", "dashed", "dotted", "dotdash", "longdash","twodash"
  ScoreP <-
    ggplot(ScoreData, aes(x=x, y=y)) +
    geom_segment(aes(x=x, xend=x, y=0, yend=y), 
                 color="blue",
                 linetype="solid") +
    geom_point(color="red",
               size=4,
               fill=alpha("orange", 0.3),
               alpha=0.6,
               shape=20,
               stroke=2) +
    theme_light() +
    coord_flip() +
    theme(panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()) +
    xlab("Node")+
    ylab("Score")
  # main("Node Score")
  # ScoreP
  return(ScoreP)
})


#' ===========================================================
#' @description Get the igraph object
#' ===========================================================
net_object <- reactive({
  nodes <- netNodes_TP()
  edges <- netEdges_TP()
  #'========================================
  #'@description check the Validation
  #'========================================
  validate(
    need(!is.null(nodes),message = "Nodes' data are not found!"),
    need(!is.null(edges),message = "Edges' data are not found!")
  )
  net_Obj <- igraph_Build(nodes,edges)
  return(net_Obj)
})

#' =============================================================
#' @description Form the network data for topology analysis
#'==============================================================
networkDT_TP <- 
  # reactive({
  function(){
  projectNT <- ProjectGet()
  
  SP_Data <- Load_SP()
  TFFL_Data <- Load_TFFL()
  MFFL_Data <- Load_MFFL()
  CFFL_Data <- Load_CFFL()
  
  SP_Pair <- CR_Switcher("SP",SP_Data)
  TFFL_Pair <- CR_Switcher("TFFL",TFFL_Data)
  MFFL_Pair <- CR_Switcher("MFFL",MFFL_Data)
  CFFL_Pair <- CR_Switcher("CFFL",CFFL_Data)
  networkData <- rbind(SP_Pair,TFFL_Pair,MFFL_Pair,CFFL_Pair)
  networkData <- networkData[!duplicated(networkData),]
  validate(
    need(!is.null(networkData),
         message = "No network data available!"
    )
  )
  validate(
    need(nrow(networkData)!=0,
         message = "No network data available!"
    )
  )
  return(networkData)   
# })
}
  
#' =============================================================
#' @description GET net nodes
#' get networkdata from networkDT_TP()
#'==============================================================
netNodes_TP <- 
  function(){
  # isolate({
  # reactive({
    net <- networkDT_TP()
    # print(net)
    # print(is.null(net))
    #'========================================
    #'@description check the Validation
    #'========================================
    validate(
      need(!is.null(net),
           message = "Can not find any network with current input,
           please try again!")
      )
    nodes <- netNodes(net)
    # print(nodes)
    return(nodes)
}
#   })
# })

#' =============================================================
#' @description GET net edges
#'=============================================================
netEdges_TP <- 
  # isolate({
  # reactive({
  function(){
    net <- networkDT_TP()
    validate(
      need(!is.null(net),
           message = "Can not find any network with current input,
           please try again!")
      )
    nodes <- netNodes_TP()
    validate(
      need(!is.null(nodes),
           message = "Can not find any network with current input,
           please try again!")
      )
    edges <- netEdges(nodes,net)
    return(edges)
}
#   })
# })

#'=============================================================
#'@description get the triple data topology score
#'@import triple co-regulatory data
#'@allcol T/F control whether to show all columns
#'@export triple co-regulatory data with its topology score including
#'degree, hub score, authority score
#'=============================================================
tri_Score <- function(tri_Data,allcol=F){
  if(is.null(tri_Data)){return(NULL)}
  if(nrow(tri_Data)==0){return(NULL)}
  net_obj <- net_object()
  #'===========================================================
  #'@description calculate the degree
  #'===========================================================
  deg <- degree(net_obj, mode="all")
  if(length(deg)==0){return(NULL)}
  degree_dt <- data.frame(Nodes=names(deg),Degree=deg,
                          row.names = NULL,stringsAsFactors = F)
  tri_Data <- dplyr::left_join(tri_Data,degree_dt,by=c("Genes"="Nodes"))%>%
    dplyr::rename("Genes_Degree"="Degree")
  tri_Data <- dplyr::left_join(tri_Data,degree_dt,by=c("miRNAs"="Nodes"))%>%
    dplyr::rename("miRNAs_Degree"="Degree")
  tri_Data <- dplyr::left_join(tri_Data,degree_dt,by=c("TFs"="Nodes"))%>%
    dplyr::rename("TFs_Degree"="Degree")
  tri_Data$Sum_Degree <- tri_Data[,"Genes_Degree"] + tri_Data[,"miRNAs_Degree"] + tri_Data[,"TFs_Degree"]
  #'===========================================================
  #' @description calculate the hub score
  #' ==========================================================
  hubscore <- hub_score(net_obj, weights=NA)$vector
  if(length(hubscore)==0){return(NULL)}
  hs_dt <- data.frame(Nodes=names(hubscore),Score = hubscore,
                      row.names = NULL,stringsAsFactors = F)
  hs_dt$Score <- round(hs_dt$Score,5)
  tri_Data <- dplyr::left_join(tri_Data,hs_dt,by=c("Genes"="Nodes"))%>%
    dplyr::rename("Genes_Hub"="Score")
  tri_Data <- dplyr::left_join(tri_Data,hs_dt,by=c("miRNAs"="Nodes"))%>%
    dplyr::rename("miRNAs_Hub"="Score")
  tri_Data <- dplyr::left_join(tri_Data,hs_dt,by=c("TFs"="Nodes"))%>%
    dplyr::rename("TFs_Hub"="Score")
  tri_Data$Sum_Hub <- tri_Data[,"Genes_Hub"] + tri_Data[,"miRNAs_Hub"] + tri_Data[,"TFs_Hub"]
  #'===========================================================
  #' @description calculate the authority score
  #' ==========================================================
  ausscore <- authority_score(net_obj, weights=NA)$vector
  # validate(
  #   need(length(ausscore)!=0,
  #        message = "No data available!")
  # )
  if(length(ausscore)==0){return(NULL)}
  aus_dt <- data.frame(Nodes=names(ausscore),Score = ausscore,
                       row.names = NULL,stringsAsFactors = F)
  aus_dt$Score <- round(aus_dt$Score,5)
  tri_Data <- dplyr::left_join(tri_Data,aus_dt,by=c("Genes"="Nodes"))%>%
    dplyr::rename("Genes_Aus"="Score")
  tri_Data <- dplyr::left_join(tri_Data,aus_dt,by=c("miRNAs"="Nodes"))%>%
    dplyr::rename("miRNAs_Aus"="Score")
  tri_Data <- dplyr::left_join(tri_Data,aus_dt,by=c("TFs"="Nodes"))%>%
    dplyr::rename("TFs_Aus"="Score")
  tri_Data$Sum_Authority <- tri_Data[,"Genes_Aus"] + tri_Data[,"miRNAs_Aus"] + tri_Data[,"TFs_Aus"]
  #'===========================================================
  #' @description choose whether to show all columns
  #' ==========================================================
  if(!isTRUE(allcol)){
    tri_Data <- tri_Data[,c("Genes","miRNAs","TFs",
                            "Sum_Degree","Sum_Hub","Sum_Authority",
                            "P_TG","P_TM","P_MG",
                            "R_TG","R_TM","R_MG",
                            "TG_Source","TM_Source","MG_Source","MT_Source")]
    tri_Data <- tri_Data[order(tri_Data$Sum_Degree,decreasing = T),]
    rownames(tri_Data) <- NULL
    return(tri_Data)
  }else{
    return(tri_Data)
  }
}
# Network_TopoBasic <- function(){
#   net_obj <- net_object()
#   ed <- edge_density(net_obj)
#   ed <- round(ed,5)
# }
#' ========================================================
#' @description download the network information
#' @import actionbutton id:TopoDownload
#' @export a model dialog with multiple tables can be downloaded
#' =======================================================
observeEvent(input$TopoDownload, {
  showModal(modalDialog(
    title = "Network information",
    size = c("m"),
    easyClose = T,
    fade = T,
    uiOutput("TopoModal"),
    footer = p(modalButton("Dismiss"),align="center")
    # box(
    #   title = "Current analyzed network",
    #   width = "7",
    #   height = "auto",
    #   status = "info",
    #   solidHeader = T,
    #   collapsible = F,
    #   collapsed = F,
    #   uiOutput("TopoModal")
    # ),
   
  ))
})
output$TopoModal <- renderUI({
  fluidPage(
    # fluidRow(
    #   modalButton("Dismiss")
    # ),
    fluidRow(
      box(
          title = "Currently analyzed network",
          width = "12",
          height = "auto",
          status = "info",
          solidHeader = T,
          collapsible = F,
          collapsed = F,
          DT::dataTableOutput("Net_Cur_DT")
        ),
      box(
        title = "Resources information",
        width = "12",
        height = "auto",
        status = "info",
        solidHeader = T,
        collapsible = F,
        collapsed = F,
        DT::dataTableOutput("Net_CurRS_DT")
      )
      # DT::dataTableOutput("Net_Cur_DT")
      # actionButton("Topo_close", "Close")
    )
 
  )
})
#' ========================================================
#' @description download current analyzed network
#' =======================================================
output$Net_Cur_DT <- DT::renderDataTable(server = F,{
  networkTP <- networkDT_TP()
  need(!is.null(networkTP),
       message = "NULL")
  DT::datatable(networkTP, 
               # class = 'cell-border stripe',
                extensions = 'Buttons',
                options = list(
                  autoWidth = T,
                  pageLength=10,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                selection =  c("none")
  )
})
output$Net_CurRS_DT <- DT::renderDataTable(server = F,{
  networkTP <- networkDT_TP()
  need(!is.null(networkTP),
       message = "NULL")
  Resource_list <- c(RegSource$TG_RS,RegSource$TM_RS,RegSource$MG_RS,RegSource$MT_RS)
  Selected <- c(
    c("ITFP")%in%Resource_list,
    c("TRRUSTv2")%in%Resource_list,
    c("HTRIdb")%in%Resource_list,
    c("TRED")%in%Resource_list,
    c("miR2Disease")%in%Resource_list,
    c("miRTarBase")%in%Resource_list,
    c("miRecord")%in%Resource_list,
    c("starBase")%in%Resource_list,
    c("TargetScan")%in%Resource_list,
    c("mirTrans")%in%Resource_list,
    c("PuTmiR")%in%Resource_list
  )
  # print(Resource_YN)
  need(!is.null(Selected),
       message = "NULL")
  need(length(Selected)!=0,
       message = "NULL")
  Resource <- c(
                '<a href="http://itfp.biosino.org/itfp">ITFP</a>',
                '<a href="www.grnpedia.org/trrust">TRRUST v2</a>',
                '<a href="http://www.lbbc.ibb.unesp.br/htri">HTRIdb</a>',
                '<a href="http://rulai.cshl.edu/TRED">TRED</a>',
                '<a href="http://www.miR2Disease.org">miR2Disease</a>',
                '<a href="http://miRTarBase.mbc.nctu.edu.tw/">miRTarBase</a>',
                '<a href="http://c1.accurascience.com/miRecords/">miRecords</a>',
                '<a href="http://starbase.sysu.edu.cn/">starBase</a>',
                '<a href="http://www.targetscan.org/vert_70/">TargetScan</a>',
                '<a href="http://mcube.nju.edu.cn/jwang/lab/soft/mirtrans/">mirTrans</a>',
                '<a href="https://www.isical.ac.in/~bioinfo_miu/TF-miRNA1.php">PuTmiR</a>'
  )
  Version <- c("Aug.2008",
               "V2.0",
               "Aug.2012",
               "Jan.2007",
               "Apr.2008",
               "V7.0",
               "Apr.2013",
               "V1.0",
               "V7.0",
               "Oct.2017",
               "V1.1"
               )
  netRS_DT <- data.frame(
    Resource,
    Version,
    Selected  
  )
  need(nrow(netRS_DT)!=0,
       message = "NULL")
  DT::datatable(netRS_DT, 
                #class = 'cell-border stripe',
                extensions = 'Buttons',
                escape = 1,
                options = list(
                  autoWidth = T,
                  pageLength=10,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                selection =  c("none")
  )
})
# output$TopoDownload <- downloadHandler(
#   filename = 'CMTCN_NetInfo.zip',
#   content = function(fname) {
#     
#     write.table(networkDT_TP(), file = "./csv1.txt")
#     write.table(networkDT_TP(), file = "./csv2.txt")
#     # write.csv(Per(), file = "csv2.csv", sep =",")
#     zip(zipfile=fname, files=c("./csv1.txt","./csv2.txt"))
#     # zip(zipfile=fname, files=c("csv1.csv","csv2.csv"))
#   },
#   contentType = "application/zip"
# )
# C_NetInfo <- function(){
#   
# }
# observeEvent(input$Topo_close, {
#   removeModal()
#   toggleDropdownButton(inputId = "TopoDownload")
# })

#'=========================================================
#'@description test table
#'=========================================================
# output$topo_Test <- DT::renderDataTable({
#   SP_Data <- Load_SP()
#   SP_Test <- tri_Score(SP_Data)
#   # print(SP_Test)
#   return(SP_Test)
# })

#'********************TAB co-regulatory interactions**********
#' ===========================================================
#' @description CRI_UI the UI of co-regulatory interactions
#' ===========================================================
output$CRI_UI <- renderUI({
  fluidPage(
    fluidRow(
      box(
        title = "Co-regulatory Pairs",
        width = "auto",
        height = "auto",
        status = "info",
        solidHeader = T,
        collapsible = F,
        collapsed = F,
        uiOutput("CRISP_UI")
      )
    ),
    fluidRow(
      box(
        title = "TF-FFLs",
        width = "auto",
        height = "auto",
        status = "info",
        solidHeader = T,
        collapsible = F,
        collapsed = F,
        uiOutput("CRITFFL_UI") 
      )
    ),
    fluidRow(
      box(
        title = "miRNA-FFLs",
        width = "auto",
        height = "auto",
        status = "info",
        solidHeader = T,
        collapsible = F,
        collapsed = F,
        uiOutput("CRIMFFL_UI") 
      )
    ),
    fluidRow(
      box(
        title = "Composite-FFLs",
        width = "auto",
        height = "auto",
        status = "info",
        solidHeader = T,
        collapsible = F,
        collapsed = F,
        uiOutput("CRICFFL_UI") 
      )
    )
  ) 
})
#' ===========================================================
#' @description CRISP_UI the UI of Significant pairs interactions
#' ===========================================================
output$CRISP_UI <- renderUI({
  fluidPage(
    column(
      width = 2,
      HTML('<center><img src=\'./img/pair.png\' 
           width="100" height="100"></center>')
    ),
    column(
      width = 10,
      DT::dataTableOutput("SP_DT") %>% 
        withSpinner(
          color =  "#32CD32",
          type = 1,
          size = 2
        )  
    )
  )
})
output$SP_DT <- renderDataTable(server = F,{
  SP_Data <- Load_SP()
  SP_Data <- tri_Score(SP_Data)
  validate(
    need(!is.null(SP_Data),
         message = "No data available!")
  )
  SP_Data <- coRegKnowledge(SP_Data)
  validate(
    need(!is.null(SP_Data),
         message = "No data available!")
  )
  SP_Data <- dplyr::select(SP_Data,"Genes","miRNAs","TFs",
                           "Sum_Degree","Sum_Hub","Sum_Authority",
                           "P_TG","P_MG",
                           "R_TG","R_MG",
                           "Knowledgebases_TF","Knowledgebases_miRNA")
  rownames(SP_Data) <- NULL
  DT::datatable(SP_Data, 
                class = 'cell-border stripe',
                extensions = 'Buttons',
                options = list(
                  autoWidth = T,
                  pageLength=10,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                selection =  c("none"))%>%
    formatStyle(
      'Sum_Degree',
      background = styleColorBar(SP_Data$Sum_Degree, '#FFF0F5'),
      backgroundSize = '100% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )%>%
    formatStyle(
      'Sum_Hub',
      background = styleColorBar(SP_Data$Sum_Hub, '#F0FFF0'),
      backgroundSize = '100% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )%>%
    formatStyle(
      'Sum_Authority',
      background = styleColorBar(SP_Data$Sum_Authority, '#FFFFF0'),
      backgroundSize = '100% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )    
})
#' ===========================================================
#' @description CRITFFL_UI the UI of TF FFL interactions
#' ===========================================================
output$CRITFFL_UI <- renderUI({
  fluidPage(
    column(
      width = 2,
      HTML('<center><img src=\'./img/TF-FFL.png\' 
           width="100" height="100"></center>')
    ),
    column(
      width = 10,
      DT::dataTableOutput("TFFL_DT") %>% 
        withSpinner(
          color =  "#32CD32",
          type = 1,
          size = 2
        )  
    )
  )
})
output$TFFL_DT <- renderDataTable(server = F,{
  TFFL_Data <- Load_TFFL()
  TFFL_Data <- tri_Score(TFFL_Data)
  validate(
    need(!is.null(TFFL_Data),
         message = "No data available!")
  )
  TFFL_Data <- coRegKnowledge(TFFL_Data)
  validate(
    need(!is.null(TFFL_Data),
         message = "No data available!")
  )
  TFFL_Data <- dplyr::select(TFFL_Data,"Genes","miRNAs","TFs",
                             "Sum_Degree","Sum_Hub","Sum_Authority",
                             "P_TG","P_TM","P_MG",
                             "R_TG","R_TM","R_MG",
                             "Knowledgebases_TF","Knowledgebases_miRNA")
  rownames(TFFL_Data) <- NULL
  DT::datatable(TFFL_Data, 
                class = 'cell-border stripe',
                extensions = 'Buttons',
                options = list(
                  autoWidth = T,
                  pageLength=10,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                selection =  c("none"))%>%
    formatStyle(
      'Sum_Degree',
      background = styleColorBar(TFFL_Data$Sum_Degree, '#FFF0F5'),
      backgroundSize = '100% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )%>%
    formatStyle(
      'Sum_Hub',
      background = styleColorBar(TFFL_Data$Sum_Hub, '#F0FFF0'),
      backgroundSize = '100% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )%>%
    formatStyle(
      'Sum_Authority',
      background = styleColorBar(TFFL_Data$Sum_Authority, '#FFFFF0'),
      backgroundSize = '100% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )  
})
#' ===========================================================
#' @description CRIMFFL_UI the UI of miRNA FFL interactions
#' ===========================================================
output$CRIMFFL_UI <- renderUI({
  fluidPage(
    column(
      width = 2,
      HTML('<center><img src=\'./img/miRNA-FFL.png\' 
           width="100" height="100"></center>')
    ),
    column(
      width = 10,
      DT::dataTableOutput("MFFL_DT") %>% 
        withSpinner(
          color =  "#32CD32",
          type = 1,
          size = 2
        )  
    )
  )
})
output$MFFL_DT <- renderDataTable(server = F,{
  MFFL_Data <- Load_MFFL()
  MFFL_Data <- tri_Score(MFFL_Data)
  validate(
    need(!is.null(MFFL_Data),
         message = "No data available!")
  )
  MFFL_Data <- coRegKnowledge(MFFL_Data)
  validate(
    need(!is.null(MFFL_Data),
         message = "No data available!")
  )
  MFFL_Data <- dplyr::select(MFFL_Data,"Genes","miRNAs","TFs",
                             "Sum_Degree","Sum_Hub","Sum_Authority",
                             "P_TG","P_TM","P_MG",
                             "R_TG","R_TM","R_MG",
                             "Knowledgebases_TF","Knowledgebases_miRNA")
  rownames(MFFL_Data) <- NULL
  DT::datatable(MFFL_Data, 
                class = 'cell-border stripe',
                extensions = 'Buttons',
                options = list(
                  autoWidth = T,
                  pageLength=10,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                selection =  c("none"))%>%
    formatStyle(
      'Sum_Degree',
      background = styleColorBar(MFFL_Data$Sum_Degree, '#FFF0F5'),
      backgroundSize = '100% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )%>%
    formatStyle(
      'Sum_Hub',
      background = styleColorBar(MFFL_Data$Sum_Hub, '#F0FFF0'),
      backgroundSize = '100% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )%>%
    formatStyle(
      'Sum_Authority',
      background = styleColorBar(MFFL_Data$Sum_Authority, '#FFFFF0'),
      backgroundSize = '100% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )  
})
#' ===========================================================
#' @description CRICFFL_UI the UI of composite FFL interactions
#' ===========================================================
output$CRICFFL_UI <- renderUI({
  fluidPage(
    column(
      width = 2,
      HTML('<center><img src=\'./img/Composite-FFL.png\' 
           width="100" height="100"></center>')
    ),
    column(
      width = 10,
      DT::dataTableOutput("CFFL_DT") %>% 
        withSpinner(
          color =  "#32CD32",
          type = 1,
          size = 2
        )  
    )
  )
})
output$CFFL_DT <- renderDataTable(server = F,{
  CFFL_Data <- Load_CFFL()
  CFFL_Data <- tri_Score(CFFL_Data)
  validate(
    need(!is.null(CFFL_Data),
         message = "No data available!")
  )
  CFFL_Data <- coRegKnowledge(CFFL_Data)
  validate(
    need(!is.null(CFFL_Data),
         message = "No data available!")
  )
  CFFL_Data <- dplyr::select(CFFL_Data,"Genes","miRNAs","TFs",
                             "Sum_Degree","Sum_Hub","Sum_Authority",
                             "P_TG","P_TM","P_MG",
                             "R_TG","R_TM","R_MG",
                             "Knowledgebases_TF","Knowledgebases_miRNA")
  rownames(CFFL_Data) <- NULL
  DT::datatable(CFFL_Data, 
                class = 'cell-border stripe',
                extensions = 'Buttons',
                options = list(
                  autoWidth = T,
                  pageLength=10,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                selection =  c("none"))%>%
    formatStyle(
      'Sum_Degree',
      background = styleColorBar(CFFL_Data$Sum_Degree, '#FFF0F5'),
      backgroundSize = '100% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )%>%
    formatStyle(
      'Sum_Hub',
      background = styleColorBar(CFFL_Data$Sum_Hub, '#F0FFF0'),
      backgroundSize = '100% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )%>%
    formatStyle(
      'Sum_Authority',
      background = styleColorBar(CFFL_Data$Sum_Authority, '#FFFFF0'),
      backgroundSize = '100% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )  
})

#'************************************************************
#'               END of Co-regulatory Interactions
#'************************************************************

#'************************************************************
#'               load co-regulatory interactions
#'************************************************************
Load_SP <- reactive({
  projectSP <- ProjectGet()
  RegText <- textParse()
  validate(
    need(length(RegSource$TG_RS) >0,
         message = "Please select TF-Gene regulation knowledgebases!"
    )
  )
  validate(
    need(length(RegSource$MG_RS) >0,
         message = "Please select miRNA-TF/Gene regulation knowledgebases!"
    )
  )
  validate(
    need(length(RegSource$MT_RS) >0,
         message = "Please select miRNA-TF/Gene regulation knowledgebases!"
    )
  )
  validate(
    need(length(RegSource$TM_RS) >0,
         message = "Please select TF-miRNA regulation knowledgebases!"
    )
  )
  if(input$NetType == "SB"){
    validate(
      need(RegText !="",
           message = "Please input the data!"
      )
    )
    validate(
      need(!is.null(RegText),
           message = "Please input the data!"
      )
    )
    validate(
      need(length(RegText)!=0,
           message = "Please input the data!"
      )
    )
  }
  validate(
    need(!is.null(projectSP),
         message = "Please select a disease!")
  )
  # print(input$SP_CK)
  validate(
    need(!is.null(input$SP_CK),
         message = "No data available!")
  )
  validate(
    need(!is.null(input$SP_TFR),
         message = "No data available!")
  )
  if(input$SP_CK==TRUE){
    if(input$SP_CT==TRUE){
      SP_Data <- Pick_SP(projectSP,input$SP_PC,input$SP_RC,input$SP_TFR)
      SP_Data <- RegMatch_SP(SP_Data,RegSource$TG_RS,RegSource$TM_RS,RegSource$MG_RS,RegSource$MT_RS)
      validate(
        need(!is.null(input$SP_PC),
             message = "No data available!")
      )
      validate(
        need(!is.null(input$SP_RC),
             message = "No data available!")
      )
      if(nrow(SP_Data)!=0){
        SP_Data <- SP_Data[,2:18]
        #return full interactions of sub-interactions
        if(input$NetType=="FI"){
          return(SP_Data)
        }else{
          SP_Data <- SP_Data[SP_Data[,"Genes"]%in%RegText|
                             SP_Data[,"TFs"]%in%RegText|
                             SP_Data[,"miRNAs"]%in%RegText,]
          return(SP_Data)
        }
      }else{
        return(NULL)
      }
    }else{
      SP_Data <- Pick_SP(projectSP,1,NULL,c("Bot"))
      SP_Data <- RegMatch_SP(SP_Data,RegSource$TG_RS,RegSource$TM_RS,RegSource$MG_RS,RegSource$MT_RS)
      if(nrow(SP_Data)!=0){
        SP_Data <- SP_Data[,2:18]
        #return full interactions of sub-interactions
        if(input$NetType=="FI"){
          return(SP_Data)
        }else{
          SP_Data <- SP_Data[SP_Data[,"Genes"]%in%RegText|
                               SP_Data[,"TFs"]%in%RegText|
                               SP_Data[,"miRNAs"]%in%RegText,]
          return(SP_Data)
        }
      }else{
        return(NULL)
      }
    }
    return(SP_Data)
  }else{
    return(NULL)
  }
})
Load_TFFL <- reactive({
  projectTFFL <- ProjectGet()
  RegText <- textParse()
  validate(
    need(length(RegSource$TG_RS) >0,
         message = "Please select TF-Gene regulation knowledgebases!"
    )
  )
  validate(
    need(length(RegSource$MG_RS) >0,
         message = "Please select miRNA-TF/Gene regulation knowledgebases!"
    )
  )
  validate(
    need(length(RegSource$MT_RS) >0,
         message = "Please select miRNA-TF/Gene regulation knowledgebases!"
    )
  )
  validate(
    need(length(RegSource$TM_RS) >0,
         message = "Please select TF-miRNA regulation knowledgebases!"
    )
  )
  if(input$NetType == "SB"){
    validate(
      need(RegText !="",
           message = "Please input the data!"
      )
    )
    validate(
      need(!is.null(RegText),
           message = "Please input the data!"
      )
    )
    validate(
      need(length(RegText)!=0,
           message = "Please input the data!"
      )
    )
  }
  validate(
    need(!is.null(projectTFFL),
         message = "Please select a disease!")
  )
  # print(input$TFFL_CK)
  validate(
    need(!is.null(input$TFFL_CK),
         message = "No data available!")
  )
  validate(
    need(!is.null(input$TFFL_TFR),
         message = "No data available!")
  )
  if(input$TFFL_CK==TRUE){
    if(input$TFFL_CT==TRUE){
      TFFL_Data <- Pick_TFFL(projectTFFL,input$TFFL_PC,input$TFFL_RC,input$TFFL_TFR)
      TFFL_Data <- RegMatch_TFFL(TFFL_Data,RegSource$TG_RS,RegSource$TM_RS,RegSource$MG_RS,RegSource$MT_RS)
      validate(
        need(!is.null(input$TFFL_PC),
             message = "No data available!")
      )
      validate(
        need(!is.null(input$TFFL_RC),
             message = "No data available!")
      )
      if(nrow(TFFL_Data)!=0){
        TFFL_Data <- TFFL_Data[,2:18]
        #return full interactions of sub-interactions
        if(input$NetType=="FI"){
          return(TFFL_Data)
        }else{
          TFFL_Data <- TFFL_Data[TFFL_Data[,"Genes"]%in%RegText|
                       TFFL_Data[,"TFs"]%in%RegText|
                       TFFL_Data[,"miRNAs"]%in%RegText,]
          return(TFFL_Data)
        }
      }else{
        return(NULL)
      }
    }else{
      TFFL_Data <- Pick_TFFL(projectTFFL,1,NULL,c("Bot"))
      TFFL_Data <- RegMatch_TFFL(TFFL_Data,RegSource$TG_RS,RegSource$TM_RS,RegSource$MG_RS,RegSource$MT_RS)
      if(nrow(TFFL_Data)!=0){
        TFFL_Data <- TFFL_Data[,2:18]
        #return full interactions of sub-interactions
        if(input$NetType=="FI"){
          return(TFFL_Data)
        }else{
          TFFL_Data <- TFFL_Data[TFFL_Data[,"Genes"]%in%RegText|
                       TFFL_Data[,"TFs"]%in%RegText|
                       TFFL_Data[,"miRNAs"]%in%RegText,]
          return(TFFL_Data)
        }
      }else{
        return(NULL)
      }
    }
    return(TFFL_Data)
  }else{
    return(NULL)
  }
})
Load_MFFL <- reactive({
  projectMFFL <- ProjectGet()
  RegText <- textParse()
  validate(
    need(length(RegSource$TG_RS) >0,
         message = "Please select TF-Gene regulation knowledgebases!"
    )
  )
  validate(
    need(length(RegSource$MG_RS) >0,
         message = "Please select miRNA-TF/Gene regulation knowledgebases!"
    )
  )
  validate(
    need(length(RegSource$MT_RS) >0,
         message = "Please select miRNA-TF/Gene regulation knowledgebases!"
    )
  )
  validate(
    need(length(RegSource$TM_RS) >0,
         message = "Please select TF-miRNA regulation knowledgebases!"
    )
  )
  if(input$NetType == "SB"){
    validate(
      need(RegText !="",
           message = "Please input the data!"
      )
    )
    validate(
      need(!is.null(RegText),
           message = "Please input the data!"
      )
    )
    validate(
      need(length(RegText)!=0,
           message = "Please input the data!"
      )
    )
  }
  validate(
    need(!is.null(projectMFFL),
         message = "Please select a disease!")
  )
  # print(input$MFFL_CK)
  validate(
    need(!is.null(input$MFFL_CK),
         message = "No data available!")
  )
  validate(
    need(!is.null(input$MFFL_TFR),
         message = "No data available!")
  )
  if(input$MFFL_CK==TRUE){
    if(input$MFFL_CT==TRUE){
      MFFL_Data <- Pick_MFFL(projectMFFL,input$MFFL_PC,input$MFFL_RC,input$MFFL_TFR)
      MFFL_Data <- RegMatch_MFFL(MFFL_Data,RegSource$TG_RS,RegSource$TM_RS,RegSource$MG_RS,RegSource$MT_RS)
      validate(
        need(!is.null(input$MFFL_PC),
             message = "No data available!")
      )
      validate(
        need(!is.null(input$MFFL_RC),
             message = "No data available!")
      )
      if(nrow(MFFL_Data)!=0){
        MFFL_Data <- MFFL_Data[,2:18]
        #return full interactions of sub-interactions
        if(input$NetType=="FI"){
          return(MFFL_Data)
        }else{
          MFFL_Data <- MFFL_Data[MFFL_Data[,"Genes"]%in%RegText|
                                 MFFL_Data[,"TFs"]%in%RegText|
                                 MFFL_Data[,"miRNAs"]%in%RegText,]
          return(MFFL_Data)
        }
      }else{
        return(NULL)
      }
    }else{
      MFFL_Data <- Pick_MFFL(projectMFFL,1,NULL,c("Bot"))
      MFFL_Data <- RegMatch_MFFL(MFFL_Data,RegSource$TG_RS,RegSource$TM_RS,RegSource$MG_RS,RegSource$MT_RS)
      if(nrow(MFFL_Data)!=0){
        MFFL_Data <- MFFL_Data[,2:18]
        #return full interactions of sub-interactions
        if(input$NetType=="FI"){
          return(MFFL_Data)
        }else{
          MFFL_Data <- MFFL_Data[MFFL_Data[,"Genes"]%in%RegText|
                                 MFFL_Data[,"TFs"]%in%RegText|
                                 MFFL_Data[,"miRNAs"]%in%RegText,]
          return(MFFL_Data)
        }
      }else{
        return(NULL)
      }
    }
    return(MFFL_Data)
  }else{
    return(NULL)
  }
})
Load_CFFL <- reactive({
  projectCFFL <- ProjectGet()
  RegText <- textParse()
 
  validate(
    need(length(RegSource$TG_RS) >0,
         message = "Please select TF-Gene regulation knowledgebases!"
    )
  )
  validate(
    need(length(RegSource$MG_RS) >0,
         message = "Please select miRNA-TF/Gene regulation knowledgebases!"
    )
  )
  validate(
    need(length(RegSource$MT_RS) >0,
         message = "Please select miRNA-TF/Gene regulation knowledgebases!"
    )
  )
  validate(
    need(length(RegSource$TM_RS) >0,
         message = "Please select TF-miRNA regulation knowledgebases!"
    )
  )
  if(input$NetType == "SB"){
    validate(
      need(RegText !="",
           message = "Please input the data!"
      )
    )
    validate(
      need(!is.null(RegText),
           message = "Please input the data!"
      )
    )
    validate(
      need(length(RegText)!=0,
           message = "Please input the data!"
      )
    )
  }
  validate(
    need(!is.null(projectCFFL),
         message = "Please select a disease!")
  )
  # print(input$CFFL_CK)
  validate(
    need(!is.null(input$CFFL_CK),
         message = "No data available!")
  )
  validate(
    need(!is.null(input$CFFL_TFR),
         message = "No data available!")
  )
  if(input$CFFL_CK==TRUE){
    if(input$CFFL_CT==TRUE){
      CFFL_Data <- Pick_CFFL(projectCFFL,input$CFFL_PC,input$CFFL_RC,input$CFFL_TFR)
      CFFL_Data <- RegMatch_CFFL(CFFL_Data,RegSource$TG_RS,RegSource$TM_RS,RegSource$MG_RS,RegSource$MT_RS)
      validate(
        need(!is.null(input$CFFL_PC),
             message = "No data available!")
      )
      validate(
        need(!is.null(input$CFFL_RC),
             message = "No data available!")
      )
      if(nrow(CFFL_Data)!=0){
        CFFL_Data <- CFFL_Data[,2:18]
        #return full interactions of sub-interactions
        if(input$NetType=="FI"){
          return(CFFL_Data)
        }else{
          CFFL_Data <- CFFL_Data[CFFL_Data[,"Genes"]%in%RegText|
                                 CFFL_Data[,"TFs"]%in%RegText|
                                 CFFL_Data[,"miRNAs"]%in%RegText,]
          return(CFFL_Data)
        }
      }else{
        return(NULL)
      }
    }else{
      CFFL_Data <- Pick_CFFL(projectCFFL,1,NULL,c("Bot"))
      CFFL_Data <- RegMatch_CFFL(CFFL_Data,RegSource$TG_RS,RegSource$TM_RS,RegSource$MG_RS,RegSource$MT_RS)
      if(nrow(CFFL_Data)!=0){
        CFFL_Data <- CFFL_Data[,2:18]
        #return full interactions of sub-interactions
        if(input$NetType=="FI"){
          return(CFFL_Data)
        }else{
          CFFL_Data <- CFFL_Data[CFFL_Data[,"Genes"]%in%RegText|
                               CFFL_Data[,"TFs"]%in%RegText|
                               CFFL_Data[,"miRNAs"]%in%RegText,]
          return(CFFL_Data)
        }
      }else{
        return(NULL)
      }
    }
    return(CFFL_Data)
  }else{
    return(NULL)
  }
})

#'********************Network Visualization********************
#' ============================================================
#' @description Visnet_UI visualization of co-regulatory network
#' ============================================================
observe({
  output$Visnet_UI <- renderUI({
    # if(length(textAreaGet$data) != 0){
      fluidPage(
        column(
          width = 2,
          uiOutput("ForceNet_Control_UI")
        ),
        column(
          width = 10,
          uiOutput("ForceNet_UI")
        )
      )
  })
})
#' *******************************************************
#' @description ForceNet_Control_UI
#'========================================================
output$ForceNet_Control_UI <- renderUI({
  fluidPage(
    fluidRow(
      uiOutput("Condition_SP"),
      uiOutput("Condition_TFFL"),
      uiOutput("Condition_MFFL"),
      uiOutput("Condition_CFFL")
    ),
    fluidRow(
      # helpText(
      #   "The network layout is controlled by \"Link Distance\" and 
      #   \"Charge\".The link distance is the distance between the links in pixels.
      #   The charge indicates the strength of the node repulsion (negative value)"
      # ),
      sliderInput(inputId = "ForceNet_LD", 
                  label = "Link Distance", 
                  min=10, 
                  max=500, 
                  value=80,
                  step = 5,
                  width = "200px"),
      sliderInput(inputId = "ForceNet_CG", 
                  label = "Charge (Node Repulsion)", 
                  min=-100, 
                  max=0, 
                  value=-25,
                  step = 10,
                  width = "200px")
    ),
    fluidRow(
      radioButtons(inputId = "NetScheme",
                   label = p(HTML("<p>Network Color Scheme:</p>")),
                   choices = c("Sky" = "sky",
                               "Snow" = "snow",
                               "Steel" = "steel",
                               "Sunshine" = "sunshine"),
                   selected = "sky",
                   inline = T
      )
    ),
    fluidRow(
      actionButton(inputId = "ForceNet_Reset",
                   label = "Reset",
                   width = "150px",
                   icon = shiny::icon("glyphicon glyphicon-refresh",lib = "glyphicon"),
                   class = "btn-warning"
      )
    ),
    br(),
    fluidRow(
      downloadButton(outputId = "NetDownload",
                     label = "Save the network",
                     class = "btn-danger"
      )
    )
    
  )
})
output$Condition_SP <- renderUI({
  if(input$SP_CK==T){
    if(Vis_Network$SP_Scale>=100){
      sliderInput(inputId = "SP_Display", 
                  label = "Visible Co-regulatory Pairs", 
                  min=1, 
                  max=Vis_Network$SP_Scale, 
                  value=100,
                  step = 10,
                  width = "200px")
    }
  }
})
output$Condition_TFFL <- renderUI({
  if(input$TFFL_CK==T){
    if(Vis_Network$TFFL_Scale>=100){
      sliderInput(inputId = "TFFLs_Display", 
                  label = "Visible TF-FFLs", 
                  min=1, 
                  max=Vis_Network$TFFL_Scale, 
                  value=100,
                  step = 10,
                  width = "200px")
    }
  }
})
output$Condition_MFFL <- renderUI({
  if(input$MFFL_CK==T){
    if(Vis_Network$MFFL_Scale>=100){
      sliderInput(inputId = "MFFL_Display", 
                  label = "Visible miRNA-FFLs", 
                  min=1, 
                  max=Vis_Network$MFFL_Scale, 
                  value=100,
                  step = 10,
                  width = "200px")
    }
  }
})
output$Condition_CFFL <- renderUI({
  if(input$CFFL_CK==T){
    if(Vis_Network$CFFL_Scale>=100){
      sliderInput(inputId = "CFFL_Display", 
                  label = "Visible Composite-FFLs", 
                  min=1, 
                  max=Vis_Network$CFFL_Scale, 
                  value=100,
                  step = 10,
                  width = "200px")
    }
  }
})
 
#' =============================================================
#' @description ForceNet_UI: the UI of force network
#' =============================================================
output$ForceNet_UI <- renderUI({
  fluidPage(
    #the error message style
    tags$head(
      tags$style(HTML("
                      .shiny-output-error-validation{
                      color: #EED2EE;
                      font-size:20px;
                      }
                      "))
      ),
    fluidRow(
      box(
        width = "auto",
        height = "auto",
        status = "info",
        # uiOutput("condition_level"),
        # uiOutput("condition_scale"),
        forceNetworkOutput("CoRegForceNet",
                           width = "100%",
                           height = "1000px"
        ),
        #set the background of the network display
        style = myScheme$Scheme
      )
  
    )
  )
})  
 
#' =============================================================
#' @description Network Scheme control
#' =============================================================
observe({
  if(!is.null(input$NetScheme)){
    if(input$NetScheme=="sky"){
      myScheme$Scheme <- c("background-color:#191970;")
    }
    if(input$NetScheme=="snow"){
      myScheme$Scheme <- c("background-color:#FFFAFA;")
    }
    if(input$NetScheme=="steel"){
      myScheme$Scheme <- c("background-color:#C1CDC1;")
    }
    if(input$NetScheme=="sunshine"){
      myScheme$Scheme <- c("background-color:#FFFACD;")
    }
  }
})

#' =============================================================
#' @description Network download as png
#' =============================================================
# download
output$NetDownload <- downloadHandler(
  # file name
  # filename <- 'myNetwork.html',
  filename <- 'myNetwork.html',
  # content
  content = function(file){
    html <- ForceGraph(input$ForceNet_LD,input$ForceNet_CG)
    saveNetwork(html,file,selfcontained=TRUE)
  }
)

#' =============================================================
#' Display the network 
#' @import package dplyr
#' @import package networkD3
#' @function ForceGraph() return a force-directed graph
#' @param LD the linkdistance
#' @param CG the charge
#' =============================================================
# observeEvent(input$Build_Act|input$Forcenet_Reset,{
output$CoRegForceNet <- renderForceNetwork({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...',{
      myForceGraph <- ForceGraph(input$ForceNet_LD,input$ForceNet_CG)
    })
})
# })
ForceGraph <- function(LD,CG){
  nodes <- netNodesACT()
  edges <- netEdgesACT()
  # print("perform ForceGraph")
  #'========================================
  #'@description check the Validation
  #'========================================
  validate(
    need(!is.null(nodes),message = "Nodes' data are not found!"),
    need(!is.null(edges),message = "Edges' data are not found!")
  )
  #set the node colors:order is gene TFs and miRNA
  nodeColors <- 'force.alpha(1);force.restart();d3.scaleOrdinal().range(["#FFC125","#FF0000","#00CD00"]);'
  #set the edge color
  # edgeColors <- ifelse(edges$edgeType==1,"#FF00FF","#00FFFF")
  edgeColors <- c()
  edgeColors[which(edges$EdgeType==1)] <- "#FF00FF"
  edgeColors[which(edges$EdgeType==2)] <- "#00FFFF"
  myforceplot <- 
    forceNetwork(Links = edges, 
                 Nodes = nodes, 
                 Source = "StartPoints",
                 Target = "EndPoints",
                 NodeID = "name",
                 Group = "group",
                 Value = "width",
                 Nodesize =  "size",
                 colourScale = JS(nodeColors),
                 fontSize = 8,
                 fontFamily = "serif",
                 linkDistance = LD,
                 # linkDistance = 100,
                 radiusCalculation = JS("d.nodesize+5"),
                 # charge = -30,
                 charge = CG,
                 linkColour = edgeColors,
                 legend = T,
                 arrows = T,
                 opacity = 1,
                 zoom = T,
                 bounded = F,
                 opacityNoHover = 1,
                 clickAction = NULL)
  # lickAction = 'Shiny.onInputChange("id", d.name)')
  return(myforceplot)
}
 
#' =============================================================
#' @description Form the network data 
#' @param threshold reactive value to store the maximum interactions to display
#' @param NetScale the current network scale
#' @note default input$mylevel is null, check it
#'==============================================================
# threshold <- reactiveValues(mythreshold = 0,NetScale = 0)
networkDT <- function(){
  # reactive({
  projectNT <- ProjectGet()
  # RegText <- textParse()
  # validate(
  #   need(length(RegText)!=0,
  #        message = "NULL"
  #   )
  # )
  # if(input$Tab_Network=="Network Visualization"){
    SP_Data <- Load_SP()
    TFFL_Data <- Load_TFFL()
    MFFL_Data <- Load_MFFL()
    CFFL_Data <- Load_CFFL()
  #' threshold if necessary***************************
    #order by degree
    SP_Data <- tri_Score(SP_Data)
    TFFL_Data <- tri_Score(TFFL_Data)
    MFFL_Data <- tri_Score(MFFL_Data)
    CFFL_Data <- tri_Score(CFFL_Data)
    
    #significant pairs
    if(!is.null(SP_Data)){
      Vis_Network$SP_Scale <- nrow(SP_Data)
      if(is.null(input$SP_Display)){
        SP_Threshold <- 100
      }else{
        SP_Threshold <- input$SP_Display
      }
      if(nrow(SP_Data) <= SP_Threshold){
        SP_Data <- SP_Data
      }else{
        SP_Data <- SP_Data[1:SP_Threshold,]
      }
    }
    #TF-FFLs
    if(!is.null(TFFL_Data)){
      Vis_Network$TFFL_Scale <- nrow(TFFL_Data)
      if(is.null(input$TFFL_Display)){
        TFFL_Threshold <- 100
      }else{
        TFFL_Threshold <- input$TFFL_Display
      }
      if(nrow(TFFL_Data) <= TFFL_Threshold){
        TFFL_Data <- TFFL_Data
      }else{
        TFFL_Data <- TFFL_Data[1:TFFL_Threshold,]
      }
    }
    #miRNA-FFLs
    if(!is.null(MFFL_Data)){
      Vis_Network$MFFL_Scale <- nrow(MFFL_Data)
      if(is.null(input$MFFL_Display)){
        MFFL_Threshold <- 100
      }else{
        MFFL_Threshold  <- input$MFFL_Display
      }
      if(nrow(MFFL_Data) <= MFFL_Threshold){
        MFFL_Data <- MFFL_Data
      }else{
        MFFL_Data <- MFFL_Data[1:MFFL_Threshold,]
      }
    }
    #Composite-FFLs
    if(!is.null(CFFL_Data)){
      Vis_Network$CFFL_Scale <- nrow(CFFL_Data)
      if(is.null(input$CFFL_Display)){
        CFFL_Threshold <- 100
      }else{
        CFFL_Threshold <- input$CFFL_Display
      }
      if(nrow(CFFL_Data) <= CFFL_Threshold){
        CFFL_Data <- CFFL_Data
      }else{
        CFFL_Data <- CFFL_Data[1:CFFL_Threshold,]
      }
    }
  
  SP_Pair <- CR_Switcher("SP",SP_Data)
  TFFL_Pair <- CR_Switcher("TFFL",TFFL_Data)
  MFFL_Pair <- CR_Switcher("MFFL",MFFL_Data)
  CFFL_Pair <- CR_Switcher("CFFL",CFFL_Data)
  networkData <- rbind(SP_Pair,TFFL_Pair,MFFL_Pair,CFFL_Pair)
  networkData <- networkData[!duplicated(networkData),]
  validate(
    need(!is.null(networkData),
         message = "No network data available!"
    )
  )
  validate(
    need(nrow(networkData)!=0,
         message = "No network data available!"
    )
  )
  return(networkData)   
}
# })

#' =============================================================
#' @description GET net nodes
#' get networkdata from networkDT()
#' distinguish Gene TF miRNA
#'==============================================================
netNodesACT <- function(){ 
  # isolate({
  # reactive({
    net <- networkDT()
    # print(net)
    # print(is.null(net))
    #'========================================
    #'@description check the Validation
    #'========================================
    validate(
      need(!is.null(net),
           message = "Can not find any network with current input,
           please try again!")
      )
    nodes <- netNodes(net)
    # print(nodes)
    return(nodes)
}
#   })
# })

#' =============================================================
#' @description GET net edges
#'=============================================================
netEdgesACT <- function(){
  # isolate({
  # reactive({
    net <- networkDT()
    validate(
      need(!is.null(net),
           message = "Can not find any network with current input,
                      please try again!")
    )
    nodes <- netNodesACT()
    # write.table(nodes,"./nodes.txt",quote = F)
    validate(
      need(!is.null(nodes),
           message = "Can not find any network with current input,
                      please try again!")
    )
    edges <- netEdges(nodes,net)
    # write.table(edges,"./edges.txt",quote = F)
    # print(edges)
    return(edges)
}
#   })
# })

#'********************Gene Enrichment tab**************************
#' =============================================================
#' @description GE_UI: the UI of Gene Enrichment
#' =============================================================
# observe({
output$GE_UI <- renderUI({
    # RegText <- textParse()
    # validate(
    #   need(!is.null(RegText),
    #        message = "Please input the data!"
    #   )
    # )
    # validate(
    #   need(RegText !="",
    #        message = "Please input the data!"
    #   )
    # )
    fluidPage(
      useShinyjs(),
      fluidRow(
        column(
          width = 6,
          checkboxInput(inputId = "GE_TF",
                        label = "TFs in Co-regulatory Network",
                        value = T,
                        width = NULL),
          textAreaInput(inputId = "GETF_Input",
                        label = NULL,
                        height ="auto",
                        rows = 5,
                        resize = "vertical"
          )
          
        ),
        column(
          width = 6,
          checkboxInput(inputId = "GE_Gene",
                        label = "Genes in Co-regulatory Network",
                        value = T,
                        width = NULL
          ),
          textAreaInput(inputId = "GEGene_Input",
                        label = NULL,
                        height ="auto",
                        rows = 5,
                        resize = "vertical"
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width = 6,
          selectInput("GE_Annotation", "Annotated Gene Sets", 
                      c(
                        "Hallmark gene sets" = "Hallmark",
                        "Positional gene sets" = "Positional",
                        "Chemical and genetic pertubations" = "Cgp",
                        "Canonical pathways" = "Cp",
                        "KEGG pathways" = "KEGGpath",
                        "Biocarta pathways" = "Biocarta",
                        "Reactome pathways" = "ReactomePath",
                        "All GO annotations" = "AllGO",
                        "GO biological processes" = "GObp",
                        "GO cellular components" = "GOcc",
                        "GO molecular functions" = "GOmf",
                        "All oncogenic signatures" = "OS",
                        "All immunologic signatures" = "IS"
                      ),
                      multiple=F, 
                      selectize=FALSE,
                      width = NULL
          )
        ),
        column(
          width = 6,
          numericInput("GE_Pcutoff", 
                       label = "Pvalue Cutoff", 
                       min=0,
                       max=0.05,
                       step = 0.01,
                       value = 0.05,
                       width = NULL
          )
          
        )
        
      ),
      br(),
      fluidRow(
        p(
          actionButton(inputId = "GE_Act",label = "Submit",
                       width = "200px",
                       class = "btn-success",
                       icon = shiny::icon("glyphicon glyphicon-ok",lib = "glyphicon")
          ),
          actionButton(inputId = "GE_Clear",label = "Reset",
                       width = "200px",
                       class = "btn-danger",
                       icon = shiny::icon("glyphicon glyphicon-refresh",lib = "glyphicon")
          ),
          align="center"
        )
      ),
      hr(),
      fluidRow(
        box(
          title = "Gene Enrichment",
          width = "auto",
          height ="auto",
          status = "info",
          solidHeader = T,
          collapsible = F,
          collapsed = F,
          uiOutput("GETBL_UI") 
        )
      )
      
    )
  }) 
  
# })
 
#' =============================================================
#' @description GE_UI: the UI of GE Enrichment
#' =============================================================

output$GETBL_UI <- renderUI({
  fluidPage(
    fluidRow(
        DT::dataTableOutput("GEDT") %>% 
          withSpinner(
            color =  "#32CD32",
            type = 1,
            size = 2
          )  
    )
  )
})
observe({
input$GE_Act
output$GEDT <- DT::renderDataTable(server = F,{
  GE_DT <- GEEnrichDT()
 
  DT::datatable(GE_DT, 
                extensions = 'Buttons', 
                class = 'cell-border stripe',
                options = list(
                  pageLength=10,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                selection = c("none")
  )  
})
})
observe({
  input$Build_Act
  output$GEDT <- DT::renderDataTable({NULL})
})
#' =============================================================
#' @description Function to get the gene enrichment result
#' =============================================================
GEEnrichDT <- function(){
  TG_list <- TG_Parse()
  validate(
    need(length(TG_list)!=0,
         message = "Please input data!"
    )
  )
  validate(
    need(length(TG_list)>10,
         message = "The input list is undersize."
    )
  )
  validate(
    need(length(TG_list)<1000,
         message = "The input list is oversize."
    )
  )
  isolate({
    validate(
      need(input$GE_Pcutoff<=0.05,
           message = "Ivalid pvalue cutoff."
      )
    )
    validate(
      need(input$GE_Pcutoff>0,
           message = "Ivalid pvalue cutoff"
      )
    )
  }) 
  GE_DT <- isolate({ GE_Enrich(genelist = TG_list,
                               pCutoff = input$GE_Pcutoff,
                               annotation = input$GE_Annotation)})
   
  validate(
    need(!is.null(GE_DT),
         message = "No data available!"
    )
  )
  validate(
    need(nrow(GE_DT)>0,
         message = "No data available!"
    )
  )
  rownames(GE_DT) <- NULL
  GE_DT <- GE_DT[,c("Description","GeneRatio",
                    "BgRatio","pvalue","p.adjust",
                    "qvalue","Count","geneID")]
  colnames(GE_DT) <- c("Description","GeneRatio",
                       "BgRatio","pvalue","p.adjust",
                       "qvalue","Count","Symbol")
  # print(class(GE_DT$Symbol))
  # print(GE_DT$Symbol)
  GE_DT[,4]<- round(GE_DT[,4],5)
  GE_DT[,5]<- round(GE_DT[,5],5)
  GE_DT[,6]<- round(GE_DT[,6],5)
  GE_DT <- isolate(GE_DT[which(GE_DT$pvalue<=input$GE_Pcutoff),])
 
  return(GE_DT)
}
#' =============================================================
#' @description Update the co-regulatory gene/TF list
#' =============================================================
observe({
  TF_list <- TF_List()
  GE_list <- Ge_List()
  updateTextAreaInput(session,"GETF_Input",value =  TF_list)
  updateTextAreaInput(session,"GEGene_Input",value =  GE_list)
})
#' =============================================================
#' @description if the input is null, the TF/Gene list for enrichment is null
#' =============================================================
observeEvent(input$Build_Act,{
  RegText <- textParse()
  # print(RegText)
  if(is.null(RegText)){
    updateTextAreaInput(session,"GETF_Input",value = "",placeholder = "Please input the data!")
    updateTextAreaInput(session,"GEGene_Input",value = "",placeholder = "Please input the data!")
  }
  if(length(RegText)==0){
    updateTextAreaInput(session,"GETF_Input",value = "",placeholder = "Please input the data!")
    updateTextAreaInput(session,"GEGene_Input",value = "",placeholder = "Please input the data!")
  }
})
#' =============================================================
#' @description return the Gene/TF list
#' =============================================================
TF_List <- function(){
  # RegText <- textParse()
  # print(RegText)
  GE_SP <- Load_SP()
  GE_TFFL <- Load_TFFL()
  GE_MFFL <- Load_MFFL()
  GE_CFFL <- Load_CFFL()
  GE_TF <- c(GE_SP[,"TFs"],GE_TFFL[,"TFs"],GE_MFFL[,"TFs"],GE_CFFL[,"TFs"])
  GE_TF <- GE_TF[!duplicated(GE_TF)]
  if(!is.null(input$GE_TF)){
    if(input$GE_TF==TRUE){
      return(GE_TF)
    }else{
      GE_TF <-c("")
     return(GE_TF)
    }
  }
}
 
Ge_List <- function(){
  GE_SP <- Load_SP()
  GE_TFFL <- Load_TFFL()
  GE_MFFL <- Load_MFFL()
  GE_CFFL <- Load_CFFL()
  GE_Gene <- c(GE_SP[,"Genes"],GE_TFFL[,"Genes"],GE_MFFL[,"Genes"],GE_CFFL[,"Genes"])
  GE_Gene <- GE_Gene[!duplicated(GE_Gene)]
  if(!is.null(input$GE_Gene)){
    if(input$GE_Gene==TRUE){
      return(GE_Gene)
    }else{
      GE_Gene <-c("")
      return(GE_Gene)
    }
  }
}
#' =============================================================
#' @description Parse the co-regulatory gene/TF list
#' =============================================================
TG_Parse <- eventReactive(input$GE_Act,{
  Tlist <- unique(input$GETF_Input) 
  Tlist <- unlist(strsplit(Tlist,",")) %>%
    trimwsTxt()
  Glist <- unique(input$GEGene_Input)
  Glist <- unlist(strsplit(Glist,",")) %>%
    trimwsTxt()
  TGlist <- c(Tlist,Glist)
  return(TGlist)
  
})

#'********************miRNA Enrichment tab**************************
#' =============================================================
#' @description miRE_UI: the UI of miRNA Enrichment
#' =============================================================
observe({
output$miRE_UI <- renderUI({
  miR_list <- miR_List()
  updateTextAreaInput(session,"miRE_Input",value =  miR_list)
  fluidPage(
    useShinyjs(),
    fluidRow(
        textAreaInput(inputId = "miRE_Input",
                      label = NULL,
                      height ="auto",
                      rows = 5,
                      resize = "vertical"
        )
    ),
    br(),
    fluidRow(
      column(
        width = 6,
        selectInput("miRE_Annotation", "Annotated miRNA sets", 
                    c(
                      "Published diseases" = "DPublish",
                      "Age/Gender" = "AgeGender",
                      "Immune cells" = "Immune",
                      "miRWalk diseases" = "Disease",
                      "Pathways" ="Pathway",
                      "Gene Ontology"="GO",
                      "Organs" = "Organs"
                    ),
                    multiple=F, 
                    selectize=FALSE,
                    width = NULL
        )
      ),
      column(
        width = 6,
        numericInput("miRE_Pcutoff", 
                     label = "Pvalue cutoff", 
                     min=0,
                     max=0.05,
                     step = 0.01,
                     value = 0.05,
                     width = NULL
        )
        
      )
      
    ),
    br(),
    fluidRow(
      p(
        actionButton(inputId = "miRE_Act",label = "Submit",
                     width = "200px",
                     class = "btn-success",
                     icon = shiny::icon("glyphicon glyphicon-ok",lib = "glyphicon")
        ),
        actionButton(inputId = "miRE_Clear",label = "Reset",
                     width = "200px",
                     class = "btn-danger",
                     icon = shiny::icon("glyphicon glyphicon-refresh",lib = "glyphicon")
        ),
        align="center"
      )
    ),
    hr(),
    fluidRow(
      box(
        title = "miRNA Enrichment",
        width = "auto",
        height ="auto",
        status = "info",
        solidHeader = T,
        collapsible = F,
        collapsed = F,
        uiOutput("miRETBL_UI") 
      )
    )
    
  )
})
})
 
#' =============================================================
#' @description miRETBL_UI: the UI of miRNA Enrichment TBL
#' =============================================================

output$miRETBL_UI <- renderUI({
  fluidPage(
    fluidRow(
      DT::dataTableOutput("miREDT") %>% 
        withSpinner(
          color =  "#32CD32",
          type = 1,
          size = 2
        )   
    )
  )
})
observe({
  input$miRE_Act  
  output$miREDT <- DT::renderDataTable(server = F,{
  miR_list <- miR_Parse()
  # print(TG_list)
  # print(length(TG_list))
  validate(
    need(length(miR_list)!=0,
         message = "Please input data!"
    )
  )
  validate(
    need(length(miR_list)>10,
         message = "The input list is undersize."
    )
  )
  validate(
    need(length(miR_list)<1000,
         message = "The input list is oversize."
    )
  )
  validate(
    need(length(miR_list)<1000,
         message = "The input list is oversize."
    )
  )
 isolate({
  validate(
    need(input$miRE_Pcutoff<=0.05,
         message = "Ivalid pvalue cutoff."
    )
  )
  validate(
    need(input$miRE_Pcutoff>0,
         message = "Ivalid pvalue cutoff"
    )
  )
 })
  miRE_DT <- isolate({ miRE_Enrich(miRNAlist = miR_list,
                               pCutoff = input$miRE_Pcutoff,
                               annotation = input$miRE_Annotation)})
  validate(
    need(!is.null(miRE_DT),
         message = "No data available!"
    )
  )
  validate(
    need(nrow(miRE_DT)>0,
         message = "No data available!"
    )
  )
  rownames(miRE_DT) <- NULL
  miRE_DT <- miRE_DT[,c("Description","GeneRatio",
                    "BgRatio","pvalue","p.adjust",
                    "qvalue","Count","geneID")]
  colnames(miRE_DT) <- c("Description","miRNARatio",
                       "BgRatio","pvalue","p.adjust",
                       "qvalue","Count","Symbol")
  # print(class(miRE_DT$Symbol))
  # print(miRE_DT$Symbol)
  miRE_DT[,4]<- round(miRE_DT[,4],5)
  miRE_DT[,5]<- round(miRE_DT[,5],5)
  miRE_DT[,6]<- round(miRE_DT[,6],5)
  miRE_DT <- isolate(miRE_DT[which(miRE_DT$pvalue<=input$miRE_Pcutoff),])
  DT::datatable(miRE_DT, 
                class = 'cell-border stripe',
                extensions = 'Buttons', 
                options = list(
                  pageLength=10,
                  dom = 'Bfrtip',
                  scrollX = TRUE,
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                selection = c("none")
  )
})
})
observe({
  input$Build_Act
  output$miREDT  <- DT::renderDataTable({NULL})
})
#' =============================================================
#' @description Update the co-regulatory miRNA list
#' =============================================================
# observe({
#   miR_list <- miR_List()
#   updateTextAreaInput(session,"miRE_Input",value =  miR_list)
# })

#' =============================================================
#' @description return the miRNA list
#' =============================================================
miR_List <- reactive({
  miRE_SP <- Load_SP()
  miRE_TFFL <- Load_TFFL()
  miRE_MFFL <- Load_MFFL()
  miRE_CFFL <- Load_CFFL()
  miRE_list <- c(miRE_SP[,"miRNAs"],miRE_TFFL[,"miRNAs"],miRE_MFFL[,"miRNAs"],miRE_CFFL[,"miRNAs"])
  miRE_list <- miRE_list[!duplicated(miRE_list)]
  # print(miRE_list)
  return(miRE_list)
})

 
#' =============================================================
#' @description Parse the co-regulatory gene/TF list
#' =============================================================
miR_Parse <- eventReactive(input$miRE_Act,{
  Mlist <- unique(input$miRE_Input) 
  Mlist <- unlist(strsplit(Mlist,",")) %>%
    trimwsTxt()
  return(Mlist)
})
#'*******************************************************
#'@description reactive values
#'@notes should use"="
#'=======================================================
Vis_Network <- reactiveValues(
  SP_Scale = 0,
  TFFL_Scale = 0,
  MFFL_Scale = 0,
  CFFL_Scale = 0
)
myScheme <- reactiveValues(Scheme = "background-color:#191970;")



#'==============================================================
#' @description show full disease name
#' =============================================================
# observeEvent(input$show, {
#   showModal(modalDialog(
#     title = "Disease Details",
#     fluidPage(
#       DT::dataTableOutput("Disease_DT",width = "100%",height = "auto")
#     ),
#     easyClose = TRUE,
#     size = "m",
#     footer = p(modalButton("Dismiss"),align="center")
#   ))
# })
# output$Disease_DT <- renderDataTable({
#   DT::datatable(gdcProject[,c(1,2,4)])
# })
#' =============================================================
#' @description a series of reset functions for network 
#' visualization
#' =============================================================
observeEvent(input$ForceNet_Reset,{
  # reset("Tab_Network")
  reset("SP_CK")
  reset("SP_CT")
  reset("SP_PC")
  reset("SP_RC")
  reset("TFFL_CK")
  reset("TFFL_CT")
  reset("TFFL_PC")
  reset("TFFL_RC")
  reset("MFFL_CK")
  reset("MFFL_CT")
  reset("MFFL_PC")
  reset("MFFL_RC")
  reset("CFFL_CK")
  reset("CFFL_CT")
  reset("CFFL_PC")
  reset("CFFL_RC")
  reset("ForceNet_LD")
  reset("ForceNet_CG")
  reset("NetScheme")
  reset("SP_Display")
  reset("TFFL_Display")
  reset("MFFL_Display")
  reset("CFFL_Display")
})


#' =============================================================
#' @description a series of reset functions
#' =============================================================
observeEvent(input$Build_Act,{
  #reset the tab selection
  # reset("Tab_Network")
  #reset the control panel
  reset("SP_CK")
  reset("SP_CT")
  reset("SP_PC")
  reset("SP_RC")
  reset("TFFL_CK")
  reset("TFFL_CT")
  reset("TFFL_PC")
  reset("TFFL_RC")
  reset("MFFL_CK")
  reset("MFFL_CT")
  reset("MFFL_PC")
  reset("MFFL_RC")
  reset("CFFL_CK")
  reset("CFFL_CT")
  reset("CFFL_PC")
  reset("CFFL_RC")
  # reset("GE_TF")
  # reset("GE_Gene")
})
#' =============================================================
#' @description a series of reset functions for gene enrichment
#' =============================================================
observeEvent(input$GE_Clear,{
  reset("GE_TF")
  reset("GE_Gene")
  reset("GE_Annotation")
  reset("GE_Pcutoff")
  
})
#' =============================================================
#' @description a series of reset functions for miRNA enrichment
#' =============================================================
observeEvent(input$miRE_Clear,{
  reset("miRE_Annotation")
  reset("miRE_Pcutoff")
})