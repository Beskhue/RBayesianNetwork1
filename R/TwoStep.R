# Integrates the two fit stages below
fitTwoStep <- function(model, fitData) {
  measurementModels <- fitMeasurementModels(model, fitData)
  structuralModel <- fitStructuralModel(model, fitData, measurementModels)
  
  return(setNames(list(measurementModels, structuralModel), c('meas', 'struct')))
}


# Separately fits a model of only latent variables and
# its measuring observed variables for each
# latent variable defined in the model string.
# Define latent variables only as follows:
# lv =~ NA*var1 + var2 + ... (all variables without line breaks)
# lv ~~ 1*lv
fitMeasurementModels <- function(model, fitData) {
  
  lv <- data.frame(matrix(, nrow=nrow(fitData), ncol=0))
  pars <- data.frame(matrix(, nrow=0, ncol=3))
  fits <- list()
  names <- NULL
  
  line <- 0
  while(line < length(model))
  {
    line <- line + 1
    if(grepl('#', model[line]) || !grepl('=~', model[line])) { # if this line defines a latent variable
      next
    }
    
    # Extract variable name of this latent variable
    varName <- sub('^(\\w*)\\h*=~.*$', '\\1', model[line], perl=TRUE)
    names <- c(names, varName)
    
    # Fit measurement model for this latent variable
    smallModel <- model[line:(line+1)]
    smallFit <- sem(smallModel, data=fitData)
    fits <- c(fits, smallFit)
    
    # Extract regression parameters
    table <- parTable(smallFit)
    pars <- rbind(pars, table[table$op == '=~', c('lhs', 'rhs', 'est')])
    
    # Predict latent variable values
    lv[[varName]] <- lavPredict(smallFit, type = 'lv')
  }
  
  return(setNames(list(fits, lv, pars, names), c('fits', 'lv', 'pars', 'names')))
}

# Uses output from fitMeasurementModels to predict
# latent variable values for the given data set
compute_lv <- function(measurementModels, d) {
  lv <- data.frame(matrix(, nrow=nrow(d), ncol=0))
  
  for(i in 1:length(measurementModels$names)) {
    lv[[measurementModels$names[i]]] <- lavPredict(measurementModels$fits[i][[1]], type = 'lv', newdata=d)
  }
  return(lv)
}

# Fits the structural part of the given model string
# coming from readLines, given data and the output
# of measurementModels.
# Define models as usual, but write lv definitions on one line
# and do not mix lv variances and covariances
# (no lv1 ~~ 1*lv1 + lv2, but on separate lines).
# Different covariances can be written on one line (lv1 ~~ lv2 + lv3)
fitStructuralModel <- function(model, fitData, measurementModels) {
  
  # Remove comments and latent variable definitions & variances from model
  line <- 0
  while(line < length(model)) {
    line <- line + 1
    if(grepl('#', model[line]) || grepl('=~', model[line]) ||
       grepl('^(\\w*)\\h*~~\\h*1\\h*\\*\\h*(\\1)$', model[line], perl=TRUE)) { # x ~~ 1*x
      
      model[line] <- ''
      
      # for future ref.: grepl('^\\h*\\+?\\w*\\h*\\+', model[line], perl=TRUE) matches 2nd line of a ~ b + \n d + e
    }
  }
  
  # Combine ov and lv data
  fitData <- data.frame(fitData, measurementModels$lv)
  
  # Fit structural model
  fit <- sem(model, data=fitData, fixed.x=FALSE, orthogonal=TRUE)
  
  # Extract parameters
  pars <- parTable(fit)
  pars <- pars[pars$op == '~' | (pars$op == '~~' & pars$lhs != pars$rhs), c('lhs', 'op', 'rhs', 'est')]
  
  return(setNames(list(fit, pars), c('fit', 'pars')))
}