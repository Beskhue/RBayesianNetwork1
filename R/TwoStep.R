# Separately fits a model of only latent variables and
# its measuring observed variables for each
# latent variable defined in the model string
fitMeasurementModels <- function(model, fitData) {
  
  lv <- data.frame(matrix(, nrow=nrow(fitData), ncol=0))
  pars <- data.frame(matrix(, nrow=0, ncol=3))
  fits <- list()
  names <- NULL
  
  line <- 1
  while(line < length(model))
  {
    line <- line + 1
    if(grepl('#', model[line]) || !grepl('=~', model[line])) { # if this line defines a latent variable
      next
    }
    
    # Extract variable name of this latent variable
    varName <- sub('(\\w*)\\h*=~.*', '\\1', model[line], perl=TRUE)
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
  
  return(setNames(list(lv, pars, fits, names), c('lv', 'pars', 'fits', 'names')))
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
