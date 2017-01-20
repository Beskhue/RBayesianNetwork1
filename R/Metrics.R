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

make_undirected = function(x) {
  x <- as.undirected(x, mode = "mutual")
  return(x)
}

get_shares_index = function(graph) { # N.B. use on original graph, not make_undirected output
  return(which(graph$graphAttributes$Nodes$labels %in% 'shares'));
}

# Calculate various metrics for the three graph
# Output a data frame with a column for each graph, and a row for each metric
calculate_metrics = function(lavaan.graph, glasso.graph, pc.graph) {
  lavaan.share_index <- get_shares_index(lavaan.graph)
  glasso.share_index <- get_shares_index(glasso.graph)
  pc.share_index <- get_shares_index(pc.graph)
  
  lavaan.graph <- remove_edge_weights(lavaan.graph)
  glasso.graph <- remove_edge_weights(glasso.graph)
  pc.graph <- remove_edge_weights(pc.graph)
  
  lavaan.graph_u <- lavaan.graph
  glasso.graph_u <- glasso.graph
  pc.graph_u <- make_undirected(pc.graph)
  
  metrics <- data.frame(lavaan = numeric(), glasso = numeric(), pc = numeric())
  
  smallworld <- data.frame(row.names = c("smallworld"),
    lavaan = smallworldIndex(lavaan.graph)$index, 
    glasso = smallworldIndex(glasso.graph)$index,
    pc = smallworldIndex(pc.graph)$index)
  metrics <- rbind(metrics, smallworld)
  
  hub <- data.frame(row.names = c("hub"),
    lavaan = hub.score(lavaan.graph)$vector[lavaan.share_index],
    glasso = hub.score(glasso.graph)$vector[glasso.share_index],
    pc = hub.score(pc.graph)$vector[pc.share_index])
  metrics <- rbind(metrics, hub)
  
  return(metrics)
}
