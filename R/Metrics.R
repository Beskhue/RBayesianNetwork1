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

# Turn a directed graph into an undirected graph
make_undirected = function(x) {
  x <- as.undirected(x, mode = "collapse")
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
  perc_directed <- mean(pc.graph$Edgelist$directed)
  
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
  
  hub <- data.frame(row.names = c("shares_hub"),
    lavaan = hub.score(lavaan.graph)$vector[lavaan.share_index],
    glasso = hub.score(glasso.graph)$vector[glasso.share_index],
    pc = hub.score(pc.graph)$vector[pc.share_index])
  metrics <- rbind(metrics, hub)
  
  authority <- data.frame(row.names = c("shares_authority"),
    lavaan = authority_score(lavaan.graph)$vector[lavaan.share_index],
    glasso = authority_score(glasso.graph)$vector[glasso.share_index],
    pc = authority_score(pc.graph)$vector[pc.share_index])
  metrics <- rbind(metrics, authority)
  
  clustcoefs <- data.frame(row.names = c("clustered"),
    lavaan = colMeans(clustcoef_auto(lavaan.graph_u)),
    glasso = colMeans(clustcoef_auto(glasso.graph_u)),
    pc = colMeans(clustcoef_auto(pc.graph_u)))
  metrics <- rbind(metrics, clustcoefs)
  
  degrees <- data.frame(row.names = c("avg_dir_degree", "perc_directed"),
    lavaan = c(mean(centrality_auto(lavaan.graph)$node.centrality[,3]), 0),
    glasso = c(mean(centrality_auto(glasso.graph)$node.centrality[,3]), 0),
    pc = c(mean(centrality_auto(pc.graph)$node.centrality[,3]), perc_directed))
  metrics <- rbind(metrics, degrees)
  
  return(metrics)
}
