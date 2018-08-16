output$Download_ui <- renderUI({
  fluidPage(
    style = "background-color:#F0FFFF;",
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
        
        selectizeInput(inputId = "Project_Download",
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
        ),
        p(
          downloadButton(outputId = "Download_Act",label = "Download",
                       width = "200px",
                       class = "btn-success"
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


output$Download_Act <- downloadHandler(
  # file name
  # filename <- 'myNetwork.html',
  filename = function() {
    paste(input$Project_Download, ".csv", sep = "")
  },
  
  # content
  content = function(file){
    coData <-  loadDatasets()
    write.csv(coData, file, row.names = FALSE)
  }
)
 loadDatasets <- reactive({
   Disease <- input$Project_Download
   sqlite <- dbDriver("SQLite")
   con <- dbConnect(sqlite,"./Database/CoMotif.db")
   TableName <- paste0(Disease,"CMotif")
   coData <- dbReadTable(con,TableName)
   dbDisconnect(con)
   return(coData)
 })