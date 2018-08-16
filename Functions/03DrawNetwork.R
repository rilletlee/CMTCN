#' =============================================================
#' @description Functions to Draw Gene-TF-miRNA network 
#' @author Rillet Lee
#' =============================================================
 

#' =============================================================
#' @function netNodes
#' @description Get all nodes of the combined network
#' @note nodes id starts from 0
#' @import 
#' @param networkData a network data including the interaction of source and target
#' @export
#' @param nodes a dataframe including three columns
#' column1 name,colunm2 id, column3 group,column4 size
#' @example 
#' TripleData1 <- Pick_SP("ACC")
#' TripleData2 <- Pick_TFFL("ACC")
#' TripleData3 <- Pick_MFFL("ACC")
#' TripleData4 <- Pick_CFFL("ACC")
#' Pairdata1 <- CR_Switcher("SP",TripleData1)
#' Pairdata2 <- CR_Switcher("TFFL",TripleData2)
#' Pairdata3 <- CR_Switcher("MFFL",TripleData3)
#' Pairdata4 <- CR_Switcher("CFFL",TripleData4)
#' testnet <- rbind(Pairdata1,Pairdata2,Pairdata3,Pairdata4)
#' testnet <- testnet[!duplicated(testnet),]
#' networkData <- testnet
#' netnodes <- netNodes(networkData)
#' =============================================================
netNodes <- function(net){
  # ifelse(!is.null(net),networkData <- net,return(NULL))
  if(!is.null(net)){
    networkData <- net
  }else{
    return(NULL)
  }
  #' =============================================================
  #' MARK frequency of occurences of each nodes
  #' =============================================================
  nodesFreq <- data.frame(table(c(networkData$StartPoint,networkData$EndPoint)),
                          stringsAsFactors = F)
  colnames(nodesFreq) <- c("name","size") 
  nodesFreq$name <- as.character(nodesFreq$name)
  nodesFreq$size <- sapply(nodesFreq$size,function(x){x <- log2(x)})
  
  nodes <- data.frame("name"=unique(c(networkData$StartPoint,networkData$EndPoint)),
                      stringsAsFactors = F)
  nodes$id <- 0:(nrow(nodes) - 1)
  #load group info
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite,"./Database/GTM.db")
  GTM <- dbReadTable(con,"GTM")
  dbDisconnect(con)
  nodes <- dplyr::left_join(nodes,GTM,by=c("name"="name"))

  nodes$name <- as.character(nodes$name)
  nodes$group <- as.character(nodes$group)
  #' =============================================================
  #' Set nodes' size
  #' the size is positively related with its frequency of occurences
  #' =============================================================
  nodes <- dplyr::left_join(nodes,nodesFreq,by = "name")
  return(nodes)
}
#' =============================================================
#' @function netEdges
#' @description Get all edges from the relationship
#' @note nodes id starts from 0
#' @import 
#' @param net a network data including the interaction of source and target
#' @param nodes nodes data with necessary columns
#' @export
#' @param edges
#' column1
#' @example 
#' testEdges <- netEdges(nodes,networkData)
#'  testEdges2 <- netEdges(testNodes2,testGeTFMir2)
#' =============================================================
netEdges <- function(nodes,net){
  # ifelse(!is.null(net),networkData <- net,return(NULL))
  # ifelse(!is.null(nodes),nodes <- nodes,return(NULL))
  if(!is.null(net)){
    networkData <- net
  }else{
    return(NULL)
  }
  if(!is.null(nodes)){
    nodes <- nodes
  }else{
    return(NULL)
  }
  
  # networkData <- net
  # nodes <- nodes
  networkData <- networkData[,c("StartPoints","EndPoints","EdgeType")]
  nodes <- nodes[,c("name","id")]
  edges <- networkData %>%
  dplyr::left_join(nodes, by = c("StartPoints" = "name")) %>%
  dplyr::select(-StartPoints) %>%
  dplyr::rename(StartPoints = id) %>%
  dplyr::left_join(nodes, by = c("EndPoints" = "name")) %>%
  dplyr::select(-EndPoints) %>%
  dplyr::rename(EndPoints = id) 
  edges <- edges[,c("StartPoints","EndPoints","EdgeType")]
  edges[which(edges$EdgeType=="TF_Gene"|edges$EdgeType=="TF_miRNA"),"EdgeType"] <- 1
  edges[which(edges$EdgeType=="miRNA_Gene"|edges$EdgeType=="miRNA_TF"),"EdgeType"] <- 2
  edges$width <- 1
  return(edges)
}
 