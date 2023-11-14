## screen master function


screen <- function(formula, data, threshold){

  # stop msgs
  # missing data
  # constant variables(nonZeroVariance)
  # statistical tests

}

# install and/or load package
pacman::p_load(naniar)

# returns vectors of column names that have less than 40% of the data missing
missing.data <- function(data, var.names, threshold = 0.4) {
    darwinism <- c()
    for (var in var.names) {
        miss <- pct_miss(data[[var]])/100
        if (miss < threshold) {
            darwinism <- c(darwinism, var)
        }
    }
    return(darwinism)
}

#example
pct_miss(airquality[["Ozone"]])
pct_miss(airquality[["Solar.R"]])

missing.data(airquality, c("Ozone", "Solar.R"), threshold = 0.2)
missing.data(airquality, c("Ozone", "Solar.R"), threshold = 0.3)
missing.data(airquality, c("Ozone", "Solar.R"))
