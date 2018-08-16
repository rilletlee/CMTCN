#' =============================================================
#' @description ui output of Start_ui
#'==============================================================

output$Start_ui <- renderUI({
  # Add a row for the main picture
    fluidPage(
      useShinyjs(),
      style = "background-color:#F0FFFF;",
      br(),
      HTML('<center><img src=\'./img/CMTCN_Card.png\' 
           width="900" height="20"></center>'),
      br(),
      hr(),
      #' =============================================================
      #' @description Select Disease
      #'==============================================================
      fluidRow(
        column(
          width = 2
        ),
        box(
          title = c("Select Cancer Datasets"),
          width = 8,
          height = "auto",
          status = "info",
          background = "blue",
          solidHeader = T,
          selectizeInput(inputId = "Project_Build",
                         label = c(""),
                         choices = c(
                           "Adrenocortical Carcinoma (ACC)"="ACC",
                           "Bladder Urothelial Carcinoma (BLCA)"="BLCA",
                           "Breast Invasive Carcinoma (BRCA)"="BRCA",
                           "Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma (CESC)"="CESC",
                           "Cholangiocarcinoma (CHOL)"="CHOL",
                           "Colon Adenocarcinoma (COAD)"="COAD",
                           "Lymphoid Neoplasm Diffuse Large B-cell Lymphoma (DLBC)"="DLBC",
                           "Esophageal Carcinoma (ESCA)"="ESCA",
                           "Glioblastoma Multiforme (GBM)"="GBM",
                           "Head and Neck Squamous Cell Carcinoma (HNSC)"="HNSC",
                           "Kidney Chromophobe (KICH)"="KICH",
                           "Kidney Renal Clear Cell Carcinoma (KIRC)"="KIRC",
                           "Kidney Renal Papillary Cell Carcinoma (KIRP)"="KIRP",
                           "Acute Myeloid Leukemia (LAML)"="LAML",
                           "Brain Lower Grade Glioma (LGG)"="LGG",
                           "Liver Hepatocellular Carcinoma (LIHC)"="LIHC",
                           "Lung Adenocarcinoma (LUAD)"="LUAD",
                           "Lung Squamous Cell Carcinoma (LUSC)"="LUSC",
                           "Mesothelioma (MESO)"="MESO",
                           "Ovarian Serous Cystadenocarcinoma (OV)"="OV",
                           "Pancreatic Adenocarcinoma (PAAD)"="PAAD",
                           "Pheochromocytoma and Paraganglioma (PCPG)"="PCPG",
                           "Prostate Adenocarcinoma (PRAD)"="PRAD",
                           "Rectum Adenocarcinoma (READ)"="READ",
                           "Sarcoma (SARC)"="SARC",
                           "Skin Cutaneous Melanoma (SKCM)"="SKCM",
                           "Stomach Adenocarcinoma (STAD)"="STAD",
                           "Testicular Germ Cell Tumors (TGCT)"="TGCT",
                           "Thyroid Carcinoma (THCA)"="THCA",
                           "Thymoma (THYM)"="THYM",
                           "Uterine Corpus Endometrial Carcinoma (UCEC)"="UCEC",
                           "Uterine Carcinosarcoma (UCS)"="UCS",
                           "Uveal Melanoma (UVM)"="UVM" 
                         ),
                         multiple = FALSE,
                         width = "100%"
                         # options = list(placeholder = "Please Select Disease")
          )
        ),
        column(
          width = 2
        )
      ),
      #'**************************************************
      #'@description choose the regulation knowledgebases
      #'==================================================

      fluidRow(
        column(
          width = 2
        ),
        box(
          title = strong("Choose Regulatory Knowledgebases"),
          width = 8,
          height = "auto",
          solidHeader = T,
          status = "info",
          background = "blue",
          radioButtons(inputId = "Reg_Mode",
                       label =  HTML("<font size=\"4\"color=\"#EED2EE\">
                                     Choose the Knowledgebases Type
                                     </font>"),
                       choices = c(
                         "Validated" = "VL",
                         "Predicted" = "PR",
                         "Both" = "BT"
                       ),
                       selected = "BT",
                       inline = T
                       ),
          checkboxGroupInput(inputId = "checkGroup_TFGe", 
                             label = HTML("<font size=\"4\"color=\"#EED2EE\">
                                          TF-Gene Interactions
                                          </font>"),
                             choices = list("ITFP" = c('ITFP'), 
                                            "TRRUSTv2" = c('TRRUSTv2'), 
                                            "TRED" = c('TRED'),
                                            "HTRIdb" = c('HTRIdb')
                             ),
                             selected = c("ITFP","TRRUSTv2","TRED","HTRIdb"),
                             inline = T
                             ),
          checkboxGroupInput(inputId = "checkGroup_miRGe", 
                             label = HTML("<font size=\"4\"color=\"#EED2EE\">
                                          miRNA-TF/Gene Interactions
                                          </font>"), 
                             choices = list(
                               "miR2Disease" = c('miR2Disease'), 
                               "miRTarBase" = c('miRTarBase'), 
                               "miRecord" = c('miRecord'),
                               "TargetScan" = c('TargetScan'),
                               "starBase" = c('starBase')
                               # "RAID v2.0" = c('RAIDv2')
                             ),
                             selected = c("miR2Disease","miRTarBase","miRecord",'TargetScan','starBase'),
                             inline = T
                             ),
          checkboxGroupInput(inputId = "checkGroup_TFmiR", 
                             label = HTML("<font size=\"4\"color=\"#EED2EE\">
                                          TF-miRNA Interactions
                                          </font>"), 
                             choices = list(
                               "mirTrans" = c('mirTrans'),
                               "PuTmiR" = c("PuTmiR")
                             ),
                             selected = c("mirTrans","PuTmiR"),
                             inline = T
                             )
        ),
        column(
          width = 2
        )
      ),
      
      #' =========================================================
      #' @description Construction of the regulatory network
      #'==========================================================
      fluidRow(
        
        column(
          width = 2
        ),
        box(
          title = strong("Construction of the Co-regulatory Network"),
          width = 8,
          height = "auto",
          solidHeader = T,
          status = "info",
          background = "blue",
          useShinyjs(),
          radioButtons(inputId = "NetType",
                       label =  HTML("<font size=\"4\"color=\"#EED2EE\">
                                     Choose the Network Type
                                     </font>"),
                       choices = c(
                         "Full Co-regulatory Network" = "FI",
                         "Subgraph" = "SB"
                       ),
                       selected = "FI",
                       inline = T
                       ),
          
          conditionalPanel(
            condition = "input.NetType == 'SB'",
            #'=============================================
            #'@description select Input method
            #'@param inputID the ID of the input, equals to "GeneSource"
            #'=============================================
            hr(),
            radioButtons(inputId = "GeneSource",
                         label = HTML("<p>Select Input Data Source:</p>"), 
                         choices = c("Survival-related Genes" = "SR",
                                     "Drug Response-related Genes" = "DR",
                                     "Top Mutated Gene" = "TM",
                                     "Custom Input" = "CI",
                                     "Upload Data" = "UD"),
                         selected = "SR",
                         inline = F
            ),
            hr(),
            #'=============================================
            #'@description Conditional-1 Load Survival Related Gene
            #'@param inputID the ID of the input, equals to "GeneSource"
            #'=============================================
            #========================================================
            conditionalPanel(
              condition = "input.GeneSource == 'SR'",
              sliderInput("SR_Candidates", "Top Candidates", min=1, max=500, value=150),
              p(HTML("<p>&nbsp;&nbsp;Survival-related Genes:</p>"))
            ),
            #'=============================================
            #'@description Conditional-2 Load Drug Response Related Gene
            #'@param inputID the ID of the input, equals to "GeneSource"
            #'=============================================
            conditionalPanel(
              condition = "input.GeneSource == 'DR'",
              sliderInput("DR_Candidates", "Top Candidates", min=1, max=500, value=150),
              p(HTML("<p>&nbsp;&nbsp;Drug Response-related Genes:</p>"))
            ),
            #'=============================================
            #'@description Conditional-3 Top Mutated Gene
            #'@param inputID the ID of the input, equals to "GeneSource"
            #'=============================================
            conditionalPanel(
              condition = "input.GeneSource == 'TM'",
              sliderInput("TM_Candidates", "Top Candidates", min=1, max=500, value=150),
              p(HTML("<p>&nbsp;&nbsp;Top Mutated Genes:</p>"))
            ),
            #====================================================
            #'===================================================
            #'@description condition-4 Custom Input Data
            #'===================================================
            conditionalPanel(
              condition = "input.GeneSource == 'CI'",
              p(HTML("<p>&nbsp;&nbsp;Input Custom Data:</p>")),
              p(HTML("<p>&nbsp;&nbsp;Input genes/miRNAs of your interest.
                       Genes/miRNAs names should be separated by commas.</p>")),
              actionButton(inputId = "CI_Example",
                           label = "Show Example",
                           class = "btn-info")
            ),
            #'===================================================
            #'@description condition-5 Upload Data
            #'@note
            #'downloadButton(outputId, label = "Download", class = NULL, ...)
            #'===================================================
            conditionalPanel(
              condition = "input.GeneSource == 'UD'",
              p(HTML("<p>&nbsp;&nbsp;Upload Your Data:</p>")),
             
              fileInput("Build_Upload",
                        "Choose File to Upload",
                        multiple = F,
                        accept = c(
                          "text/plain"
                        ),
                        width = "100%",
                        buttonLabel = "Browse",
                        placeholder = "No file selected"
              ),
              downloadButton(outputId = "Build_Download",
                             label = "Download Sample Data",
                             class = "btn-danger"
              ),
              p(HTML("<p>&nbsp;&nbsp;Your Upload Data:</p>"))
            ),
            textAreaInput(inputId = "Build_Input",
                          label = NULL,
                          width = NULL,
                          height ="auto",
                          rows = 5,
                          placeholder = "Please input query data",
                          resize = "vertical"
            )
          ),
          p(
            actionButton(inputId = "Build_Act",label = "Submit",
                         width = "200px",
                         class = "btn-success",
                         icon = shiny::icon("glyphicon glyphicon-ok",lib = "glyphicon")
            ),
            actionButton(inputId = "Build_Clear",label = "Reset",
                         width = "200px",
                         class = "btn-danger",
                         icon = shiny::icon("glyphicon glyphicon-refresh",lib = "glyphicon")
            ),
            align="center"
          )
        ),
        column(
          width = 2
        )
    )
      
    )
})  

#'************************text area input **********************
#' =============================================================
#' @description reactive values
#' @param textAreaGet reactive value, default value is NULL
#' @param textAreaGet$data the data in the textAreaGet
#' @param myProject data store the current project
#' @function ProjectGet return the current selected project
#' @param RegSource stores selected data source
#' @function RegSourceGet return the select Regulation knowledgebases
#' =============================================================
textAreaGet <- reactiveValues(data = NULL)
myProject <- reactiveValues(data = NULL)
ProjectGet <- eventReactive(input$Build_Act,{
  validate(
    need(input$Project_Build != "",
         message = "Please select the disease!"
    )
  )
  myProject$data <- tolower(input$Project_Build)
  return(myProject$data)
})
RegSource <- reactiveValues(
  TG_RS = NULL,
  TM_RS = NULL,
  MT_RS = NULL,
  MG_RS = NULL
)
observeEvent(input$Build_Act,{
 TG_RSource <- input$checkGroup_TFGe
 TM_RSource <- input$checkGroup_TFmiR
 MG_RSource <- input$checkGroup_miRGe
 MT_RSource <- input$checkGroup_miRGe
 
 RegSource$TG_RS <- TG_RSource
 RegSource$TM_RS <- TM_RSource
 RegSource$MG_RS <- MG_RSource
 RegSource$MT_RS <- MT_RSource
})


#'==============================================================
#' @description show the expample custom data
#' =============================================================
observeEvent(input$CI_Example,{
  load(file = "./Profile/Example.Rd")
  updateTextAreaInput(session,"Build_Input",value = CI_Example$Entity)
})
#' =============================================================
#' @description data input control
#' @description Reactive Function to dispose text area input 
#' split input area string by ","
#' @function textParse 
#' judge whether textAreaGet$data is legal and return the data in 
#' textAreaGet
#' @param textAreaGet reactive value, default value is NULL
#' @param  textAreaGet$data the data in the textAreaGet
#' @import 
#' @function trimsTxt remove the space
#' Use eventReactive to delay reactions until a user clicks the action button.
#' Use two observeEvent to control submit and remove button
#' Function textParse to deal with the input data
#' =============================================================

#get input when push button submit(id=RegInputAct)
trimwsTxt <- function(txt){
  txt = gsub("^\\s+|\\s+$","",txt)
  return(txt)
}
observeEvent(input$Build_Act,{
  textAreaInput <- unique(input$Build_Input) %>%
    toupper()
  textAreaGet$data <- unlist(strsplit(textAreaInput,",")) %>%
    trimwsTxt()
})

#' =============================================================
#' @description #clear data in textAreaGet and update the text area input
#' it depends on input method:
#' if custom input clear all
#' else, only clear textAreaGet data
#' =============================================================
observeEvent(input$Build_Clear,{
  textAreaGet$data <- NULL
  if(input$GeneSource == "CI"){
    updateTextAreaInput(session, "Build_Input", value = "")
  }
  if(input$GeneSource == "UD"){
    updateTextAreaInput(session, "Build_Input", value = "")
  }
})
observeEvent(input$GeneSource == "CI",{
  textAreaGet$data <- NULL
  updateTextAreaInput(session, "Build_Input", value = "",placeholder = "Please input your data!")
})

observeEvent(input$NetType == "FI",{
  textAreaGet$data <- NULL
  updateRadioButtons(session,inputId = "GeneSource",selected = "Custom Input")
  # updateTextAreaInput(session, "Build_Input", value = "")
})
textParse <- reactive({
  validate(
    need(length(textAreaGet$data) <= 500,
         message <- "The input query is oversize.Please check the input!"
    )
  )
  return(textAreaGet$data)
})

#' =============================================================
#' @description parse upload data
#' @param RegInFile the read-in file id, it is a data frame with 'name',
#' 'size', 'type', and 'datapath' columns. The 'datapath'column will 
#' contain the local filenames where the data can
#  be found.
#' @note input$Regupload will be NULL initially 
#' =============================================================
observeEvent(input$Build_Upload,{
  RegInFile <- input$Build_Upload
  #check the file type
  uploadType <- RegInFile$type
  if( uploadType !="text/plain"){
    updateTextAreaInput(session, 
                        "Build_Input", 
                        placeholder = "Invalid file type!")
  }
  validate(
    need(
      uploadType =="text/plain",
      message = "Invalid file type!"
    )
  )
  if (is.null(RegInFile))
    return(NULL)
  # print(RegInFile$datapath)
  RegInPath <- normalizePath(RegInFile$datapath,winslash = "\\")
  uploadData <- tryCatch(read.table(RegInPath, 
                                    header = F,
                                    sep = "\n", 
                                    encoding = "UTF-8",
                                    stringsAsFactors = F),
                         error=function(e){
                           NULL
                         }
  )
  validate(
    need(nrow(uploadData!=0),
         message = "Invalid upload Data")
  )
  validate(
    need(nrow(uploadData > 500),
         message = "Oversize upload Data")
  )
  colnames(uploadData) <- c("Entity")
  myUpload <- uploadData$Entity
  updateTextAreaInput(session, 
                      "Build_Input", 
                      value = myUpload)
})

#' =============================================================
#' @description Handle the sample data download
#' =============================================================
output$Build_Download <- downloadHandler(
  filename = function() {
    "SCANNERSAMPLE.txt"
  },
  content = function(file) {
    file.copy("./Profile/SCANNERSAMPLE.txt",file)
  }
)

#' =============================================================
#' @description Load the example data
#' update text area input of regulation net query
#' =============================================================
observe({
  if(!is.null(input$GeneSource)){
    if(input$GeneSource == "SR"|
       input$GeneSource == "DR"|
       input$GeneSource == "TM"){
          phenoData <- GeneInput()
          if(!is.null(phenoData)){
            # CandidateN <- input$BuildCandidates
            if(input$GeneSource == "SR"){CandidateN <- input$SR_Candidates}
            if(input$GeneSource == "DR"){CandidateN <- input$DR_Candidates}
            if(input$GeneSource == "TM"){CandidateN <- input$TM_Candidates}
            phenoDataLen <- nrow(phenoData)
            if(CandidateN <= phenoDataLen){
              RegQueryList <- phenoData[1:CandidateN,1]
            }else{
              RegQueryList <- phenoData[1:phenoDataLen,1]
            }
            updateTextAreaInput(session,"Build_Input",value = RegQueryList)
          }else{
            updateTextAreaInput(session,"Build_Input",
                                value = "",
                                placeholder = "Data currently are not available!")
          }
    }
  }
})

#' =============================================================
#' Load phenotype database
#' based on the selective input
#' @param Pheno_Build: phenotype input id 
#' @param Project_Build: project input id  
#' @param TableName: the table name in the query db
#' test: 
#' PHdbQuery = "./Database/surmiRNA.db"
#' projectPH = "LAML"
#' =============================================================
GeneInput <- isolate({
  reactive({
    sqlite <- dbDriver("SQLite")
    #query databae based on the selective input
    if(is.null(input$GeneSource)){
      return(NULL)
    } 
    if(input$GeneSource == 'SR'){
      GEdbQuery <- c("./Database/surRNA.db")
    }
    if(input$GeneSource == 'DR'){
      GEdbQuery <- c("./Database/drugRNA.db")
    }
    if(input$GeneSource == 'TM'){
      GEdbQuery <- c("./Database/TopMutated.db")
    }
    
    conPH <- dbConnect(sqlite,GEdbQuery)
    #query the table name and check 
    TNquery <- c("SELECT NAME FROM sqlite_master WHERE TYPE='table' ORDER BY NAME;")
    TableName <- dbGetQuery(conPH,TNquery)
    validate(
      need(input$Project_Build != "",
           message = "Please select the disease!"
      )
    )
    projectPH <- tolower(input$Project_Build)
    if(projectPH %in% TableName$name){
      phenoData <- dbReadTable(conPH,projectPH)
      dbDisconnect(conPH)
      return(phenoData)
    }else{
      dbDisconnect(conPH)
      return(NULL)
    }
  })
})

observeEvent(input$Build_Clear,{
  reset("Project_Build")
  reset("NetType")
  reset("Reg_Mode")
  reset("GeneSource")
  reset("checkGroup_TFGe")
  reset("checkGroup_miRGe")
  reset("checkGroup_TFmiR")
})


#'*******************************************************
#'@description jump to the network tab when push the submit button
#'*******************************************************
observeEvent(input$Build_Act,{
  updateTabsetPanel(session,"NavyID",selected = "Analysis")
})
#'*******************************************************
#'@description reset teh tabbox selection when push the submit button
#'*******************************************************
observeEvent(input$Build_Act,{
  updateTabsetPanel(session,"Tab_Network",selected = "Network Visualization")
})
#'*******************************************************
#'@description control the database type selection
#'*******************************************************
observe({
  if(!is.null(input$Reg_Mode)){
    if(input$Reg_Mode=='VL'){
      updateCheckboxGroupInput(session,"checkGroup_TFGe",selected = c("TRRUSTv2","HTRIdb","ITFP"))
      updateCheckboxGroupInput(session,"checkGroup_miRGe",selected = c("miRTarBase","miRecord","miR2Disease"))
      updateCheckboxGroupInput(session,"checkGroup_TFmiR",selected = c("mirTrans"))
    }
    if(input$Reg_Mode=="PR"){
      updateCheckboxGroupInput(session,"checkGroup_TFGe",selected = c("TRED"))
      updateCheckboxGroupInput(session,"checkGroup_miRGe",selected = c("starBase","TargetScan"))
      updateCheckboxGroupInput(session,"checkGroup_TFmiR",selected = c("PuTmiR"))
    }
    if(input$Reg_Mode =="BT"){
      updateCheckboxGroupInput(session,"checkGroup_TFGe",selected = c("TRRUSTv2","HTRIdb","ITFP","TRED"))
      updateCheckboxGroupInput(session,"checkGroup_miRGe",selected = c("miRTarBase","miRecord","miR2Disease","starBase","TargetScan"))
      updateCheckboxGroupInput(session,"checkGroup_TFmiR",selected = c("mirTrans","PuTmiR"))
    }
  }
})
 
