print("this is the file that'll run tests")

#categorical to quantitative--------
cp_test=function(data, dep, indep){
  # data (dataframe) 
  # dep (str): dependent variable
  # indep (str): independent variable
  numeric_var=ifelse(is.numeric(data[dep]), dep, indep)
  cate_var=ifelse(is.numeric(data[dep]), indep, dep)
  result0=aov(data[[numeric_var]] ~ data[[cate_var]])
  result1=unlist(summary(result0))[9]
  if (result1 < 0.05) {
    return(indep)
  }else{NULL}
}

#example
data("mtcars")
mtcars$cyl=as.character(mtcars$cyl)
x=cp_test(mtcars, 'mpg', 'cyl')
print(x)
