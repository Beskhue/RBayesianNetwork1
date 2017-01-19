library(glasso)
library(pcalg)
library(qgraph)
source('ReadData.R')
source('Preprocess.R')

d <- readData()
d <- remove_data_errors(d)
d <- remove_empty_articles(d)
d <- preprocess(d)
d <- filter_for_structural_inference(d) # remove all data but var.s we specified

covariance <- cov(d$fit)
graph <- glasso(covariance, 0.1)
qg <- qgraph(graph, label.prop=0.9, label.norm='LDA 00', labels=get_display_names_for_structural_inference(), label.scale.equal=FALSE)

correlation <- cov2cor(covariance)
p <- pc(suffStat = list(C = correlation, n = nrow(d$fit)), p = ncol(d$fit), 
        alpha = 1e-20, conservative = TRUE, solve.confl = TRUE, u2pd = 'relaxed', indepTest = gaussCItest);

qg <- qgraph(p, label.prop=0.9, label.norm='LDA 00', labels=get_display_names_for_structural_inference(), label.scale.equal=FALSE)

