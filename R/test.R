library(lavaan)
library(semPlot)
source('ReadData.R')
source('Preprocess.R')
source('TwoStep.R')
source('Inference.R')

d <- preprocess(readData())

fitData <- d[['fit']];
testData <- d[['test']];

# Read in Lavaan model
model <- readLines('model_all.lav')

# Fit measurement model
measurementModels <- fitMeasurementModels(model, fitData)
pars <- measurementModels$pars; fits <- measurementModels$fits; lv <- measurementModels$lv


# Compute latent variable values of test data
lvtest <- compute_lv(measurementModels, testData)

# Run lavaan
#fit <- sem(model, data=fitData, orthogonal=TRUE)

# Output model summary
#summary(fit, standardized=TRUE)

# Plot diagram
#semPaths(fit, nCharNodes=10, sizeMan=10, sizeLat=16, sizeInt=4)

