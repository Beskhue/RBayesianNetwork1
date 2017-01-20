library(qgraph)

remove_edge_weights = function(x) {
  if ("qgraph"%in%class(x)) x <- as.igraph(x)
  if (!all(E(x)$weight==1)) {
    E(x)$weight[] <- 1
  }
  return(x)
}

calculate_metrics = function(lavaan.graph, glasso.graph, pc.graph) {
  lavaan.graph <- remove_edge_weights(lavaan.graph)
  glasso.graph <- remove_edge_weights(glasso.graph)
  pc.graph <- remove_edge_weights(pc.graph)
  
  metrics <- data.frame(lavaan = numeric(), glasso = numeric(), pc = numeric())
  
  smallworld <- data.frame(row.names = c("smallworld"),
    lavaan = smallworldIndex(lavaan.graph)$index, 
    glasso = smallworldIndex(glasso.graph)$index,
    pc = smallworldIndex(pc.graph)$index)
  metrics <- rbind(metrics, smallworld)
  
  return(metrics)
}
