library(lavaan)
library(semPlot)
source('ReadData.R')
source('Preprocess.R')
source('Inference.R')

d <- readData()
d <- remove_data_errors(d)
d <- remove_empty_articles(d)
d <- preprocess(d)

fitData <- d[['fit']]
testData <- d[['test']]

# Simple model:
model <- readLines('model_simple.lav')
fit <- sem(model, data = fitData)

chisq <- fitMeasures(fit, 'chisq')
cat(sprintf('Chi-squared of structural fit: %g\n', chisq))

error <- test_error(fit, d = testData)
cat(sprintf('Mean absolute error on log scale: %g\n', error))

semPaths(fit, nCharNodes=10, sizeMan=10, sizeLat=16, sizeInt=4)
