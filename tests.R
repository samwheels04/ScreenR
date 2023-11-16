tests <- function(vars, y, data, sig = 0.05) {
  significant_vars <- c()
  for (var in vars) {
    corr <- test(var, y, data)
    if (corr >= sig) {
      append(significant_vars, var)
    }
  }
  return (significant_vars)
}

test <- function(var, y, data) {
  is_numeric_var <- is_numeric(var)
  is_numeric_y <- is_numeric(y)
  if (is_numeric_var & is_numeric_y) {
    
  } else if (!is_numeric_var & !is_numeric_y) {
    
  } else {
    
  }
}

test.QQ <- function(y, x) {
    cor <- cor.test(x, y)
    return(cor$p.value)
}

#categorical to quantitative--------
cp_test=function(data, dep, indep){
  # data (dataframe) 
  # dep (str): dependent variable
  # indep (str): independent variable
  numeric_var <- ifelse(is.numeric(data[dep]), dep, indep)
  cate_var <- ifelse(is.numeric(data[dep]), indep, dep)
  result0 <- aov(data[[numeric_var]] ~ data[[cate_var]])
  result1 <- unlist(summary(result0))[9]
  return (result1)
}

#example
data("mtcars")
mtcars$cyl=as.character(mtcars$cyl)
x=cp_test(mtcars, 'mpg', 'cyl')
print(x)

