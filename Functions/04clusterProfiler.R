#'=============================================================
#' @description Gene set enrichment
#' @import genelist 
#'=============================================================
GE_Enrich <- function(genelist,pCutoff=0.05,pAdjust="BH",annotation){
  #load annotation data
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite,"./Database/Enrichment.db")
  Anno_Set <- dbReadTable(con,annotation)
  dbDisconnect(con)
  
  # print(Anno_Set)
  enrich_process <- enricher(genelist,
                             pvalueCutoff = pCutoff,
                             pAdjustMethod = pAdjust,
                             TERM2GENE =  Anno_Set,
                             minGSSize = 10,
                             maxGSSize = 1000
  )
  if(!is.null(enrich_process)){
    enrich_result <- enrich_process@result
   
    return(enrich_result)
  }else{
    return(NULL)
  }
}


#'=============================================================
#' @description miRNA set enrichment
#' @import miRNAlist 
#'=============================================================
miRE_Enrich <- function(miRNAlist,pCutoff=0.05,pAdjust="BH",annotation){
  #load annotation data
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite,"./Database/miREnrichment.db")
  Anno_Set <- dbReadTable(con,annotation)
  dbDisconnect(con)
  
  # print(Anno_Set)
  enrich_process <- enricher(miRNAlist,
                             pvalueCutoff = pCutoff,
                             pAdjustMethod = pAdjust,
                             TERM2GENE =  Anno_Set,
                             minGSSize = 10,
                             maxGSSize = 1000
  )
  if(!is.null(enrich_process)){
    enrich_result <- enrich_process@result
    
    return(enrich_result)
  }else{
    return(NULL)
  }
}
# GO_Enrich <- function(genelist){
#   
#  # mapping <- toTable(org.Hs.egSYMBOL)
#  mapping <- read.table("./Profile/mapping.txt",sep = "\t",header = T)
#  GL_TBL <- data.frame("gl"=genelist,stringsAsFactors = F)
#  print(GL_TBL)
#  MP_TBL <- dplyr::left_join(GL_TBL,mapping,by=c("gl"="symbol"))
#  IDlist <- MP_TBL$gene_id
#  IDlist <- IDlist[!is.na(IDlist)]
#  print(IDlist)
#  ego <-
#    clusterProfiler:: enrichGO(
#       gene          = IDlist,
#       OrgDb         = org.Hs.eg.db,
#       ont           = "CC",
#       pAdjustMethod = "BH",
#       pvalueCutoff  = 0.05,
#       qvalueCutoff  = 0.05,
#       readable      = TRUE
#   )
#  ego <- ego@result
#  return(ego)
# }

 