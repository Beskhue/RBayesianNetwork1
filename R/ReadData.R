#' Read the model data
#' 
#' It might be necessary to change your R working directory 
#' to the script location. E.g.:
#' setwd("path/to/code/R")
readData = function() {

  # Path to data file
  fpath <- file.path("..", "data", "OnlineNewsPopularity.csv");
  
  return(read.csv(fpath, header = TRUE))
}