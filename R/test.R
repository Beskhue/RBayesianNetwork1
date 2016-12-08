library(lavaan)
library(semPlot)
source('ReadData.R')
source('Preprocess.R')

d <- preprocess(readData())

fitData <- d[['fit']];
testData <- d[['test']];

# Lavaan model
#model <- '
#  # regressions
#  num_hrefs ~ n_non_stop_words;
#  num_self_hrefs ~ n_non_stop_words; # number of Mashable links 
#  n_non_stop_words ~ is_weekend;
#'
model <- readLines('model_all.lav')

# Run lavaan
fit <- sem(model, data=fitData, orthogonal=TRUE, estimator="ULS")

# Output model summary
summary(fit, standardized=TRUE)

# Plot diagram
#semPaths(fit, nCharNodes=10, sizeMan=10, sizeLat=16, sizeInt=4)

# Get regression coefficients
pars <- parTable(fit)
pars <- pars[pars$op == '~',c(4,13)]
pars <- data.frame(pars[,2], row.names=pars[,1])

