library(glasso)
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
