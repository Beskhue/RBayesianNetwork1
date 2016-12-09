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
lv <- fitMeasurementModels(model, fitData)
pars <- lv$pars; fits <- lv$fits; lv <- lv$lv

# Run lavaan
#fit <- sem(model, data=fitData, orthogonal=TRUE)

# Output model summary
#summary(fit, standardized=TRUE)

# Plot diagram
#semPaths(fit, nCharNodes=10, sizeMan=10, sizeLat=16, sizeInt=4)

