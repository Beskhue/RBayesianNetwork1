library(igraph)
library(qgraph)

# Remove edge weights from all edges
# (From qgraph 1.4.1)
remove_edge_weights = function(x) {
  if ("qgraph"%in%class(x)) x <- as.igraph(x)
  if (!all(E(x)$weight==1)) {
    E(x)$weight[] <- 1
  }
  return(x)
}

# Calculate various metrics for the three graph
# Output a data frame with a column for each graph, and a row for each metric
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
