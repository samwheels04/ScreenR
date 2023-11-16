## screen master function


# An example to use throughout, will remove later
library(ggplot2)
data(mpg)
data <- mpg
formula <- hwy ~ cty + displ
threshold = 0.4

screen <- function(formula, data, threshold = 0.4) {
  
  # Stop messages -----------------------------------------------------------
  
  # Stop message for data frame
  if (!is.data.frame(data)) {
    stop("Please enter a data frame")
  }
  
  # Stop message for formula
  if (!is.formula(formula)) {
    stop("Please enter a valid formula")
  }
  
  # Character vector --------------------------------------------------------
  
  # Extracting variables from formula
  y <- as.character(formula[[2]])
  vars <- all.vars(formula)
  
  # Missing data screen -----------------------------------------------------
  
  vars <- missing.data(data, var.names = vars, threshold = threshold)
  
  # If y variable does not pass missing data screen
  if (!(y %in% vars)) {
    stop(paste0(y, " (dependent variable) contains more than ", threshold * 100, "% missing data"))
  }
  
  # Constant variable screen ------------------------------------------------
  
  vars <- constant(vars, data)
  
  # If y variable does not pass constant screen
  if (!(y %in% vars)) {
    stop(paste0(y, " (dependent variable) is either constant or near constant"))
  }

  # Statistical tests -------------------------------------------------------

  vars <- tests(vars, data)

  # Output ------------------------------------------------------------------
  
  # Remove y variable from vars
  vars <- vars[vars != y]
  
  return(vars)
  
}

