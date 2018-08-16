#' ************************************************************
#' @description functions to pick co-regulatory interactions
#' @import Disease name
#' @export different co-regulatory interactions
#' @examples 
#' Disease <- c("BRCA")
#' SP_Data <- Pick_SP(Disease,Rcut=0.5)
#' TFFL_Data <- Pick_TFFL(Disease,Rcut=0.2)
#' MFFL_Data <- Pick_MFFL(Disease,Rcut=0.2)
#' CFFL_Data <- Pick_CFFL(Disease,Rcut=0.2)
#' ************************************************************

Pick_SP <- function(Disease,Pcut=0.05,Rcut=NULL){
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite,"./Database/CoMotif.db")
  TableName <- paste0(Disease,"CMotif")
  queryADJ <- c("000110202")
  DataQuery <- paste0("SELECT * FROM ",TableName,
                    " WHERE ADJ=",shQuote(queryADJ),";")
  SP_Data <- dbGetQuery(con,DataQuery)
  dbDisconnect(con)
  SP_Data$P_TG <- as.numeric(SP_Data$P_TG)
  SP_Data$P_TM <- as.numeric(SP_Data$P_TM)
  SP_Data$P_MG <- as.numeric(SP_Data$P_MG)
  SP_Data$R_TG <- as.numeric(SP_Data$R_TG)
  SP_Data$R_TM <- as.numeric(SP_Data$R_TM)
  SP_Data$R_MG <- as.numeric(SP_Data$R_MG)
  if(is.null(Rcut)){
    SP_Data <- SP_Data[which(SP_Data$P_TG<=Pcut&
                               SP_Data$P_MG<=Pcut),]
  }else{
    SP_Data <- SP_Data[which(SP_Data$P_TG<=Pcut&
                             SP_Data$P_MG<=Pcut&
                             abs(SP_Data$R_TG)>=Rcut&
                             abs(SP_Data$R_MG)>=Rcut&
                             SP_Data$R_MG < 0
                        ),]
  }
  return(SP_Data) 
}

Pick_TFFL <- function(Disease,Pcut=0.05,Rcut=NULL){
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite,"./Database/CoMotif.db")
  TableName <- paste0(Disease,"CMotif")
  queryADJ <- c("000111202")
  DataQuery <- paste0("SELECT * FROM ",TableName,
                      " WHERE ADJ=",shQuote(queryADJ),";")
  TFFL_Data <- dbGetQuery(con,DataQuery)
  dbDisconnect(con)
  TFFL_Data$P_TG <- as.numeric(TFFL_Data$P_TG)
  TFFL_Data$P_TM <- as.numeric(TFFL_Data$P_TM)
  TFFL_Data$P_MG <- as.numeric(TFFL_Data$P_MG)
  TFFL_Data$R_TG <- as.numeric(TFFL_Data$R_TG)
  TFFL_Data$R_TM <- as.numeric(TFFL_Data$R_TM)
  TFFL_Data$R_MG <- as.numeric(TFFL_Data$R_MG)
  if(is.null(Rcut)){
    TFFL_Data <- TFFL_Data[which(TFFL_Data$P_TG<=Pcut&
                               TFFL_Data$P_TM<=Pcut&
                               TFFL_Data$P_MG<=Pcut),]
  }else{
    TFFL_Data <- TFFL_Data[which(TFFL_Data$P_TG<=Pcut&
                               TFFL_Data$P_TM<=Pcut&
                               TFFL_Data$P_MG<=Pcut&
                               abs(TFFL_Data$R_TG)>=Rcut&
                               abs(TFFL_Data$R_TM)>=Rcut&
                               abs(TFFL_Data$R_MG)>=Rcut&
                               TFFL_Data$R_MG < 0
    ),]
  }
  return(TFFL_Data) 
}

Pick_MFFL <- function(Disease,Pcut=0.05,Rcut=NULL){
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite,"./Database/CoMotif.db")
  TableName <- paste0(Disease,"CMotif")
  queryADJ <- c("000110222")
  DataQuery <- paste0("SELECT * FROM ",TableName,
                      " WHERE ADJ=",shQuote(queryADJ),";")
  MFFL_Data <- dbGetQuery(con,DataQuery)
  dbDisconnect(con)
  MFFL_Data$P_TG <- as.numeric(MFFL_Data$P_TG)
  MFFL_Data$P_TM <- as.numeric(MFFL_Data$P_TM)
  MFFL_Data$P_MG <- as.numeric(MFFL_Data$P_MG)
  MFFL_Data$R_TG <- as.numeric(MFFL_Data$R_TG)
  MFFL_Data$R_TM <- as.numeric(MFFL_Data$R_TM)
  MFFL_Data$R_MG <- as.numeric(MFFL_Data$R_MG)
  if(is.null(Rcut)){
    MFFL_Data <- MFFL_Data[which(MFFL_Data$P_TG<=Pcut&
                               MFFL_Data$P_TM<=Pcut&
                               MFFL_Data$P_MG<=Pcut),]
  }else{
    MFFL_Data <- MFFL_Data[which(MFFL_Data$P_TG<=Pcut&
                               MFFL_Data$P_TM<=Pcut&
                               MFFL_Data$P_MG<=Pcut&
                               abs(MFFL_Data$R_TG)>=Rcut&
                               abs(MFFL_Data$R_TM)>=Rcut&
                               abs(MFFL_Data$R_MG)>=Rcut&
                               MFFL_Data$R_MG < 0
    ),]
  }
  return(MFFL_Data) 
}

Pick_CFFL <- function(Disease,Pcut=0.05,Rcut=NULL){
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite,"./Database/CoMotif.db")
  TableName <- paste0(Disease,"CMotif")
  queryADJ <- c("000111222")
  DataQuery <- paste0("SELECT * FROM ",TableName,
                      " WHERE ADJ=",shQuote(queryADJ),";")
  CFFL_Data <- dbGetQuery(con,DataQuery)
  dbDisconnect(con)
  CFFL_Data$P_TG <- as.numeric(CFFL_Data$P_TG)
  CFFL_Data$P_TM <- as.numeric(CFFL_Data$P_TM)
  CFFL_Data$P_MG <- as.numeric(CFFL_Data$P_MG)
  CFFL_Data$R_TG <- as.numeric(CFFL_Data$R_TG)
  CFFL_Data$R_TM <- as.numeric(CFFL_Data$R_TM)
  CFFL_Data$R_MG <- as.numeric(CFFL_Data$R_MG)
  if(is.null(Rcut)){
    CFFL_Data <- CFFL_Data[which(CFFL_Data$P_TG<=Pcut&
                               CFFL_Data$P_TM<=Pcut&
                               CFFL_Data$P_MG<=Pcut),]
  }else{
    CFFL_Data <- CFFL_Data[which(CFFL_Data$P_TG<=Pcut&
                               CFFL_Data$P_TM<=Pcut&
                               CFFL_Data$P_MG<=Pcut&
                               abs(CFFL_Data$R_TG)>=Rcut&
                               abs(CFFL_Data$R_TM)>=Rcut&
                               abs(CFFL_Data$R_MG)>=Rcut&
                               CFFL_Data$R_MG < 0
    ),]
  }
  return(CFFL_Data) 
}

#'**************************************************
#'@function RegMatch
#'@description parse the co-regulatory data
#'match the selected regulation knowledgebases
#' @import TripleData the co-regulatory data
#' @import Regsource the selected regulation knowledgebases
#' @export Triple_Match matched TripleData
#' @example 
#' Disease <- c("BRCA")
#' SP_Data <- Pick_SP(Disease,Rcut=0.1)
#' TFFL_Data <- Pick_TFFL(Disease,Rcut=0.1)
#' MFFL_Data <- Pick_MFFL(Disease,Rcut=0.1)
#' CFFL_Data <- Pick_CFFL(Disease,Rcut=0.1)
#' TG_RSource <- c("TRRUSTv2")
#' TM_RSource <- c("mirTrans")
#' MG_RSource <- c("miRecord")
#' MT_RSource <- c("miRTarBase")
#' regmatch1 <- RegMatch_SP(SP_Data,TG_RSource,TM_RSource,MG_RSource,MT_RSource)
#' regmatch2 <- RegMatch_TFFL(TFFL_Data,TG_RSource,TM_RSource,MG_RSource,MT_RSource)
#' regmatch3 <- RegMatch_MFFL(MFFL_Data,TG_RSource,TM_RSource,MG_RSource,MT_RSource)
#' regmatch4 <- RegMatch_CFFL(CFFL_Data,TG_RSource,TM_RSource,MG_RSource,MT_RSource)
#'==================================================
#'
RegMatch_SP <- function(TripleData,TG_RSource,TM_RSource,MG_RSource,MT_RSource){
  if(is.null(TripleData)){return(NULL)}
  if(length(TG_RSource)==0){return(NULL)}
  if(length(TM_RSource)==0){return(NULL)}
  if(length(MG_RSource)==0){return(NULL)}
  if(length(MT_RSource)==0){return(NULL)}
  #'SP TripleData*********************************************
  # TripleData_SP <- TripleData[which(TripleData$ADJ=='000110202'),]
  TripleData_SP <- TripleData 
  #TF-Gene  
  TG_Site <- c()
  for(j in 1:length(TG_RSource)){
    TG_match <- grep(pattern = TG_RSource[j],x = TripleData_SP[,"TG_Source"])
    TG_Site <- c(TG_Site,TG_match)
  }
  TG_Site <- TG_Site[!duplicated(TG_Site)]
  
  #miRNA-Gene
  MG_Site <- c()
  for(j in 1:length(MG_RSource)){
    MG_match <- grep(pattern = MG_RSource[j],x = TripleData_SP[,"MG_Source"])
    MG_Site <- c(MG_Site,MG_match)
  }
  MG_Site <- MG_Site[!duplicated(MG_Site)]
  SP_Site <-  Reduce(intersect, list(v1 = TG_Site,v2 = MG_Site)) 
  TripleData_SP <- TripleData_SP[SP_Site,]
  return(TripleData_SP)
  # TripleData <- rbind(TripleData_SP,TripleData_TFFL,TripleData_MFFL,TripleData_CFFL)
  # return(TripleData)
}

RegMatch_TFFL <- function(TripleData,TG_RSource,TM_RSource,MG_RSource,MT_RSource){
  if(is.null(TripleData)){return(NULL)}
  if(length(TG_RSource)==0){return(NULL)}
  if(length(TM_RSource)==0){return(NULL)}
  if(length(MG_RSource)==0){return(NULL)}
  if(length(MT_RSource)==0){return(NULL)}
  #'TFFL TripleData*********************************************
  # TripleData_TFFL <- TripleData[which(TripleData$ADJ=='000111202'),]
  TripleData_TFFL <- TripleData 
  #TF-Gene  
  TG_Site <- c()
  for(j in 1:length(TG_RSource)){
    TG_match <- grep(pattern = TG_RSource[j],x = TripleData_TFFL[,"TG_Source"])
    TG_Site <- c(TG_Site,TG_match)
  }
  TG_Site <- TG_Site[!duplicated(TG_Site)]
  
  #miRNA-Gene
  MG_Site <- c()
  for(j in 1:length(MG_RSource)){
    MG_match <- grep(pattern = MG_RSource[j],x = TripleData_TFFL[,"MG_Source"])
    MG_Site <- c(MG_Site,MG_match)
  }
  MG_Site <- MG_Site[!duplicated(MG_Site)]
  
  #TF-miRNA
  TM_Site <- c()
  for(j in 1:length(TM_RSource)){
    TM_match <- grep(pattern = TM_RSource[j],x = TripleData_TFFL[,"TM_Source"])
    TM_Site <- c(TM_Site,TM_match)
  }
  TM_Site <- TM_Site[!duplicated(TM_Site)]
  
  TFFL_Site <-  Reduce(intersect, list(v1 = TG_Site,v2 = MG_Site,v3 = TM_Site)) 
  TripleData_TFFL <- TripleData_TFFL[TFFL_Site,]
  return(TripleData_TFFL)
}

RegMatch_MFFL <- function(TripleData,TG_RSource,TM_RSource,MG_RSource,MT_RSource){
  if(is.null(TripleData)){return(NULL)}
  if(length(TG_RSource)==0){return(NULL)}
  if(length(TM_RSource)==0){return(NULL)}
  if(length(MG_RSource)==0){return(NULL)}
  if(length(MT_RSource)==0){return(NULL)}
  #'MFFL TripleData*********************************************
  # TripleData_MFFL <- TripleData[which(TripleData$ADJ=='000110222'),]
  TripleData_MFFL <- TripleData
  #TF-Gene  
  TG_Site <- c()
  for(j in 1:length(TG_RSource)){
    TG_match <- grep(pattern = TG_RSource[j],x = TripleData_MFFL[,"TG_Source"])
    TG_Site <- c(TG_Site,TG_match)
  }
  TG_Site <- TG_Site[!duplicated(TG_Site)]
  
  #miRNA-Gene
  MG_Site <- c()
  for(j in 1:length(MG_RSource)){
    MG_match <- grep(pattern = MG_RSource[j],x = TripleData_MFFL[,"MG_Source"])
    MG_Site <- c(MG_Site,MG_match)
  }
  MG_Site <- MG_Site[!duplicated(MG_Site)]
  
  #miRNA-TF
  MT_Site <- c()
  for(j in 1:length(MT_RSource)){
    MT_match <- grep(pattern = MT_RSource[j],x = TripleData_MFFL[,"MT_Source"])
    MT_Site <- c(MT_Site,MT_match)
  }
  MT_Site <- MT_Site[!duplicated(MT_Site)]
  
  MFFL_Site <-  Reduce(intersect, list(v1 = TG_Site,v2 = MG_Site,v3 = MT_Site)) 
  TripleData_MFFL <- TripleData_MFFL[MFFL_Site,]
  return(TripleData_MFFL)
}

RegMatch_CFFL <- function(TripleData,TG_RSource,TM_RSource,MG_RSource,MT_RSource){
  if(is.null(TripleData)){return(NULL)}
  if(length(TG_RSource)==0){return(NULL)}
  if(length(TM_RSource)==0){return(NULL)}
  if(length(MG_RSource)==0){return(NULL)}
  if(length(MT_RSource)==0){return(NULL)}
  #'CFFL TripleData*********************************************
  # TripleData_CFFL <- TripleData[which(TripleData$ADJ=='000111222'),]
  TripleData_CFFL <- TripleData 
  #TF-Gene  
  TG_Site <- c()
  for(j in 1:length(TG_RSource)){
    TG_match <- grep(pattern = TG_RSource[j],x = TripleData_CFFL[,"TG_Source"])
    TG_Site <- c(TG_Site,TG_match)
    
  }
  TG_Site <- TG_Site[!duplicated(TG_Site)]
  
  #TF-miRNA
  TM_Site <- c()
  for(j in 1:length(TM_RSource)){
    TM_match <- grep(pattern = TM_RSource[j],x = TripleData_CFFL[,"TM_Source"])
    TM_Site <- c(TM_Site,TM_match)
     
  }
  TM_Site <- TM_Site[!duplicated(TM_Site)]
  #miRNA-Gene
  MG_Site <- c()
  for(j in 1:length(MG_RSource)){
    MG_match <- grep(pattern = MG_RSource[j],x = TripleData_CFFL[,"MG_Source"])
    MG_Site <- c(MG_Site,MG_match)
     
  }
  MG_Site <- MG_Site[!duplicated(MG_Site)]
  
  #miRNA-TF
  MT_Site <- c()
  for(j in 1:length(MT_RSource)){
    MT_match <- grep(pattern = MT_RSource[j],x = TripleData_CFFL[,"MT_Source"])
    MT_Site <- c(MT_Site,MT_match)
     
  }
  MT_Site <- MT_Site[!duplicated(MT_Site)]
  
  CFFL_Site <-  Reduce(intersect, list(v1 = TG_Site,v2 = TM_Site,v3 = MG_Site,v4 = MT_Site)) 
  TripleData_CFFL <- TripleData_CFFL[CFFL_Site,]
  return(TripleData_CFFL)
}

#'**************************************************
#'@description parse the co-regulatory data
#'display the related regulatory database
#' @example 
#' Disease <- c("BRCA")
#' SP_Data <- Pick_SP(Disease,Rcut=0.1)
#' coregSP <- coRegKnowledge(SP_Data)
#'==================================================
#'
coRegKnowledge <- function(TripleData){
  if(nrow(TripleData)==0){return(NULL)}
  Knowledgebases <- c()
  Knowledgebases <- apply(TripleData[,c("TG_Source","TM_Source","MG_Source","MT_Source")],
                          MARGIN = 1,
                          FUN = function(x){
                           paste0(x,collapse = "/")
                          })
  # print(class(Knowledgebases))
  anno <-  sapply(Knowledgebases,FUN = function(x){revise_Rep(x)})
  # print(class(anno))
  TripleData$Knowledgebases <- anno
  return(TripleData)
}

revise_Rep <- function(anno){
  anno <- unique(unlist(strsplit(anno,split = "/")))
  anno <- paste0(anno,collapse = "/")
  return(anno)
}