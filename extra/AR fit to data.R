#takes a set of data (an AR time series) and returns a set of p parameters fitted to the set by least squares
#constant is included as first parameter as well as the p coefficients/weights. I.e: p+1 parameters.

coef <- function(data,p){
  require("MASS") 
  N <- length(data)
  X <-  matrix(data = NA, nrow = N-p, ncol = p +1)
  for (i in (p+1):N) {
    data_i <- data[(i-1):(i-p)] 
    X[i-p,] <- c(1,data_i)
  }
  
  XtX <- t(X) %*% X
  XtX_inv <- ginv(XtX) ##Does this inverse always exist? Check mathematically
  sol <- XtX_inv %*% t(X) %*% as.matrix(data[(p+1):N]) 
  return(sol)
}
