#' Variance and Skewness Decomposition
#'
#'
#' @param y an outcome vector (must be numeric without NA's).
#' @param X 
#' @param wgt an optional vector of weights.
#' @param moment the moment on which the decomposition method is applied.
#'               either "variance" (second moment) or "skewness" (third moment).
#' @param year 

vs_decomp <- function(y,
                      X,
                      wgt = rep(1, length(y)),
                      moment = "skewness",
                      year = NULL){
  #input validation
  if(moment == "skewness"){
    
  } else {
    if(ncol(X) > 1)
      stop("currently, variance decomposition is only available for one variable")
  }
}