library(lavaan)
library(semPlot)

d <- preprocess(readData())

# Lavaan model
#model <- '
#  # regressions
#  num_hrefs ~ n_non_stop_words;
#  num_self_hrefs ~ n_non_stop_words; # number of Mashable links 
#  n_non_stop_words ~ is_weekend;
#'
model <- readLines('model1.lav');

# Run lavaan
fit <- sem(model, data=d)

# Output model summary
summary(fit, standardized=TRUE)

# Plot diagram
#semPaths(fit, nCharNodes=10, sizeMan=10, sizeLat=16, sizeInt=4)
