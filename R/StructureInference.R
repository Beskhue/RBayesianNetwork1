library(glasso)
library(pcalg)
library(qgraph)
source('LavaanToPlot.R')
source('ReadData.R')
source('Preprocess.R')
source('Metrics.R')

d <- readData()
d <- remove_data_errors(d)
d <- remove_empty_articles(d)
d <- preprocess(d)
d <- filter_for_structural_inference(d) # remove all data but var.s we specified

lavaan.edges <- lavaan_to_plot()
lavaan.graph <- qgraph(lavaan.edges, directed = FALSE, label.prop=0.9, label.norm='LDA 00', label.scale.equal=FALSE)

covariance <- cov(d$fit)
glasso.fit <- glasso(covariance, 0.1)
glasso.graph <- qgraph(glasso.fit, label.prop=0.9, label.norm='LDA 00', labels=abbreviate_names(get_names_for_structural_inference()), label.scale.equal=FALSE)

correlation <- cov2cor(covariance)
pc.fit <- pc(suffStat = list(C = correlation, n = nrow(d$fit)), labels=abbreviate_names(get_names_for_structural_inference()), 
        alpha = 1e-20, conservative = TRUE, solve.confl = TRUE, u2pd = 'relaxed', indepTest = gaussCItest);
pc.graph <- qgraph(pc.fit, label.prop=0.9, label.norm='LDA 00', label.scale.equal=FALSE)

metrics <- calculate_metrics(lavaan.graph, glasso.graph, pc.graph)
