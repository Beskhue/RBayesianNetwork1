library(lavaan)
library(semPlot)
source('ReadData.R')
source('Preprocess.R')
source('TwoStep.R')
source('Inference.R')
source('util.R')

d <- preprocess(readData())

fitData <- d[['fit']];
testData <- d[['test']];

# Read in Lavaan model
model <- readLines('model_all.lav')

# Fit model with two-step procedure
models <- fitTwoStep(model, fitData)

# Extract information
measpars <- models$meas$pars; measfits <- models$meas$fits; lv <- models$meas$lv
structpars <- models$struct$pars; structfit <- models$struct$fit

# Output error and statistics
chisq <- fitMeasures(structfit, 'chisq')
cat(sprintf('Chi-squared of structural fit: %g\n', chisq))
error <- test_error_two_step(models, testData)
cat(sprintf('Mean absolute error on log scale: %g\n', error))

# Output model summary
#summary(structfit, standardized=TRUE)

# for reference: how to plot SEM diagram
#semPaths(fit, nCharNodes=10, sizeMan=10, sizeLat=16, sizeInt=4)

