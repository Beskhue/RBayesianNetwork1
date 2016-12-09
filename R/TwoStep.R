fitMeasurementModels <- function(model, fitData) {
  
  lv <- data.frame(matrix(, nrow=nrow(fitData), ncol=0))
  pars <- data.frame(matrix(, nrow=0, ncol=3))
  fits <- list()
  
  line <- 1
  while(line < length(model))
  {
    line <- line + 1
    if(grepl('#', model[line]) || !grepl('=~', model[line])) { # if this line defines a latent variable
      next
    }
    
    # Extract variable name of this latent variable
    varName <- sub('(\\w*)\\h*=~.*', '\\1', model[line], perl=TRUE)
    
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
  
  return(setNames(list(lv, pars, fits), c('lv', 'pars', 'fits')))
}
