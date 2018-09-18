#'************************Network Topology Analysis************
#'=============================================================
#'@description functions to analyse the network topology
#'@importMethodsFrom package igraph
#'=============================================================

#'=============================================================
#'@description build the igraph object
#'@param nodes the network nodes
#'@param edges the network links
#'@export net an igraph object
#'=============================================================

igraph_Build <- function(nodes,edges){
  nodes <- nodes[,c(2,1,3,4)]
  links <- edges[,c(1,2,4,3)]
  colnames(links) <- c("From","To","Weight","Type")
  net <- graph_from_data_frame(d=links,vertices = nodes,directed = T)
  return(net)
}

#'=============================================================
#'@description calculate the network density
#'@import
#'@param net an igraph object
#'@export ds the network density
#'=============================================================
net_EdgeCount <- function(net){
  nEC <- length(E(net))
  return(nEC)
}

net_NodeCount <- function(net){
  nNC <- length(V(net))
  return(nNC)
}
net_Density <- function(net){
  ed <- edge_density(net)
  return(ed)
}

