tests <- function(vars, y, data, sig = 0.05) {
  significant_vars <- c()
  for (var in vars) {
    corr <- test(var, y, data)
    if (corr <= sig) {
      append(significant_vars, var)
    }
  }
  return (significant_vars)
}


test <- function(var, y, data) {
  is_numeric_var <- is.numeric(var)
  is_numeric_y <- is.numeric(y)
  if (is_numeric_var & is_numeric_y) {
    pval <- test.QQ(data, y, var)
    return(pval)
  } else if (!is_numeric_var & !is_numeric_y) {
    
  } else {
    pval=cp_test(data, y, var)
    return(pval)
  }
}

test.QQ <- function(data, y, x) {
    cor <- cor.test(data[[x]], data[[y]])
    return(cor$p.value)
}

#categorical to quantitative--------
cp_test=function(data, dep, indep) {
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

summary(aov(mtcars$mpg ~ as.factor(mtcars$vs)))
result0 <- aov(mtcars$mpg ~ as.factor(mtcars$vs))
result1 <- unlist(summary(result0))[9]
result
