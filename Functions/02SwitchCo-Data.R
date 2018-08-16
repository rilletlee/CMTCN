#' ************************************************************
#' @description function to switch co-regulatory relations to binary relations
#' @import ADJ TripleData
#' @export PairData
#' @examples 
#' TripleData1 <- Pick_SP("ACC")
#' TripleData2 <- Pick_TFFL("ACC")
#' TripleData3 <- Pick_MFFL("ACC")
#' TripleData4 <- Pick_CFFL("ACC")
#' Pairdata1 <- CR_Switcher("SP",TripleData1)
#' Pairdata2 <- CR_Switcher("TFFL",TripleData2)
#' Pairdata3 <- CR_Switcher("MFFL",TripleData3)
#' Pairdata4 <- CR_Switcher("CFFL",TripleData4)
#' ************************************************************

CR_Switcher <- function(ADJ,TripleData){
    if(is.null(TripleData)){return(NULL)}
    if(nrow(TripleData)==0){return(NULL)}
    if(length(ADJ)==0){return(NULL)}
    if(ADJ==c("SP")){
      
      TG_Interaction <- TripleData[,c("TFs","Genes")]
      TG_Interaction$EdgeType <- rep("TF_Gene",nrow(TG_Interaction))
      colnames(TG_Interaction) <- c("StartPoints","EndPoints","EdgeType")
      
      MG_Interaction <- TripleData[,c("miRNAs","Genes")]
      MG_Interaction$EdgeType <- rep("miRNA_Gene",nrow(MG_Interaction))
      colnames(MG_Interaction) <- c("StartPoints","EndPoints","EdgeType")
      
      SP_Interaction <- rbind(TG_Interaction,MG_Interaction)
      #remove duplication
      SP_Interaction <- SP_Interaction[!duplicated(SP_Interaction),]
      rownames(SP_Interaction) <- NULL
      return(SP_Interaction)
    }
    if(ADJ==c("TFFL")){
      TG_Interaction <- TripleData[,c("TFs","Genes")]
      TG_Interaction$EdgeType <- rep("TF_Gene",nrow(TG_Interaction))
      colnames(TG_Interaction) <- c("StartPoints","EndPoints","EdgeType")
      
      TM_Interaction <- TripleData[,c("TFs","miRNAs")]
      TM_Interaction$EdgeType <- rep("TF_miRNA",nrow(TM_Interaction))
      colnames(TM_Interaction) <- c("StartPoints","EndPoints","EdgeType")
      
      MG_Interaction <- TripleData[,c("miRNAs","Genes")]
      MG_Interaction$EdgeType <- rep("miRNA_Gene",nrow(MG_Interaction))
      colnames(MG_Interaction) <- c("StartPoints","EndPoints","EdgeType")
      
      TFFL_Interaction <- rbind(TG_Interaction,TM_Interaction,MG_Interaction)
      #remove duplication
      TFFL_Interaction <- TFFL_Interaction[!duplicated(TFFL_Interaction),]
      rownames(TFFL_Interaction) <- NULL
      return(TFFL_Interaction)
    }
    if(ADJ==c("MFFL")){
      TG_Interaction <- TripleData[,c("TFs","Genes")]
      TG_Interaction$EdgeType <- rep("TF_Gene",nrow(TG_Interaction))
      colnames(TG_Interaction) <- c("StartPoints","EndPoints","EdgeType")
      
      MG_Interaction <- TripleData[,c("miRNAs","Genes")]
      MG_Interaction$EdgeType <- rep("miRNA_Gene",nrow(MG_Interaction))
      colnames(MG_Interaction) <- c("StartPoints","EndPoints","EdgeType")
      
      MT_Interaction <- TripleData[,c("miRNAs","TFs")]
      MT_Interaction$EdgeType <- rep("miRNA_TF",nrow(MT_Interaction))
      colnames(MT_Interaction) <- c("StartPoints","EndPoints","EdgeType")
      
      MFFL_Interaction <- rbind(TG_Interaction,MG_Interaction,MT_Interaction)
      #remove duplication
      MFFL_Interaction <- MFFL_Interaction[!duplicated(MFFL_Interaction),]
      rownames(MFFL_Interaction) <- NULL
      return(MFFL_Interaction)
    }
    if(ADJ==c("CFFL")){
      TG_Interaction <- TripleData[,c("TFs","Genes")]
      TG_Interaction$EdgeType <- rep("TF_Gene",nrow(TG_Interaction))
      colnames(TG_Interaction) <- c("StartPoints","EndPoints","EdgeType")
      
      TM_Interaction <- TripleData[,c("TFs","miRNAs")]
      TM_Interaction$EdgeType <- rep("TF_miRNA",nrow(TM_Interaction))
      colnames(TM_Interaction) <- c("StartPoints","EndPoints","EdgeType")
      
      MG_Interaction <- TripleData[,c("miRNAs","Genes")]
      MG_Interaction$EdgeType <- rep("miRNA_Gene",nrow(MG_Interaction))
      colnames(MG_Interaction) <- c("StartPoints","EndPoints","EdgeType")
      
      MT_Interaction <- TripleData[,c("miRNAs","TFs")]
      MT_Interaction$EdgeType <- rep("miRNA_TF",nrow(MT_Interaction))
      colnames(MT_Interaction) <- c("StartPoints","EndPoints","EdgeType")
      
      CFFL_Interaction <- rbind(TG_Interaction,TM_Interaction,
                                MG_Interaction,MT_Interaction)
      #remove duplication
      CFFL_Interaction <- CFFL_Interaction[!duplicated(CFFL_Interaction),]
      rownames(CFFL_Interaction) <- NULL
      return(CFFL_Interaction)
    }
  }