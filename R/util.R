# Show modification indices for a given fit
# (i=0: structural fit, i>0: measurement model)
# given the output of fitTwoStep
showModIndices <- function(models, i) {
  if(i == 0) {
    this_fit <- models$struct$fit
  }
  else {
    this_fit <- models$meas$fits[i][[1]]
  }
  chisq <- fitMeasures(this_fit, 'chisq')
  cat(sprintf('Chi-squared of this fit: %g\n', chisq))
  modificationIndices(this_fit, standardized=TRUE, sort=TRUE)
}

