## screen master function

library(ggplot2)
data(mpg)
data <- mpg
formula <- hwy ~ cty + displ
threshold = 0.4
class(data)

screen <- function(formula, data, threshold = 0.4){
  
  
  # Stop messages -----------------------------------------------------------
  
  # stop msg for data
  if (!is.data.frame(data)) {
    stop("Please enter a data frame")
  }
  
  # stop msg 
  if (!is.formula(formula)) {
    stop("Please enter a valid formula")
  }
  
  # Character vector --------------------------------------------------------
  
  y <- as.character(formula[[2]])
  vars <- all.vars(formula)
  
  
  # Missing data screen -----------------------------------------------------
  
  # missing data
  vars <- missing.data(data, var.names = vars, threshold = threshold)
  
  # Constant variable screen ------------------------------------------------
  
  # constant variables(nonZeroVariance)
  vars <- constant(vars, data)
  
  # statistical tests
  
  
}

