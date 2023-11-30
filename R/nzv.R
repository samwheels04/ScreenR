#' @title variance_check
#' @description checks the variance of the given variables and discards them if they have zero or near-zero variance
#' @param nameVec character vector of variables to check 
#' @param data the dataset 
#' @return gives a character vector of variables with high variance
#' @details Uses the \code{\link[https://www.rdocumentation.org/packages/caret/versions/6.0-92/topics/nearZeroVar]{nearZeroVar}} near zero variance function from the caret package
#' @examples 
#' \dontrun{
#' variance_check(c("mpg", "hwy), mtcars)
#' }
#' @rdname variance_check
#' @export 

variance_check <- function(nameVec, data) {
  variance_df <- data[nameVec]
  
  badVars <- const_check(variance_df)
  
  if(length(badVars) == 0){
    return(nameVec)
  }
  
  good_df <- subset(variance_df, select = -badVars)

  
  good_vars <- colnames(good_df)
  return(good_vars)
}

#copy of caret::nearZeroVar
const_check <- function(x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, names = FALSE, 
                        foreach = FALSE, allowParallel = TRUE) {
  if(!foreach) 
    return(nzv(x, freqCut = freqCut, uniqueCut = uniqueCut, saveMetrics = saveMetrics, names = names))
  `%op%` <- getOper(foreach && allowParallel && getDoParWorkers() > 1)
  if(saveMetrics) {
    res <- foreach(name = colnames(x), .combine = rbind) %op% 
      {
        r <- nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut, saveMetrics = TRUE)
        r[, "column"] <- name
        r
      }
    res <- res[, c(5, 1, 2, 3, 4)]
    rownames(res) <- as.character(res$column)
    res$column <- NULL
  }
  else {
    res <- foreach(name = colnames(x), .combine = c) %op% 
      {
        r <- nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut, 
                 saveMetrics = FALSE)
        if (length(r) > 0 && r == 1) 
          TRUE
        else FALSE
      }
    res <- which(res)
    if (names) {
      res <- colnames(x)[res]
    }
  }
  res
}

#copy of caret::nzv
nzv <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, names = FALSE) {
  if (is.null(dim(x))) 
    x <- matrix(x, ncol = 1)
  freqRatio <- apply(x, 2, function(data) {
    t <- table(data[!is.na(data)])
    if (length(t) <= 1) {
      return(0)
    }
    w <- which.max(t)
    return(max(t, na.rm = TRUE)/max(t[-w], na.rm = TRUE))
  })
  lunique <- apply(x, 2, function(data) length(unique(data[!is.na(data)])))
  percentUnique <- 100 * lunique/apply(x, 2, length)
  zeroVar <- (lunique == 1) | apply(x, 2, function(data) all(is.na(data)))
  if (saveMetrics) {
    out <- data.frame(freqRatio = freqRatio, percentUnique = percentUnique, 
                      zeroVar = zeroVar, nzv = (freqRatio > freqCut & percentUnique <= 
                                                  uniqueCut) | zeroVar)
  }
  else {
    out <- which((freqRatio > freqCut & percentUnique <= 
                    uniqueCut) | zeroVar)
    names(out) <- NULL
    if (names) {
      out <- colnames(x)[out]
    }
  }
  out
}
