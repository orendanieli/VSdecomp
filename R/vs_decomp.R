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
                      year = rep(1, length(y))){
  #input validation
  if(moment == "skewness"){
    
  } else {
    if(ncol(X) > 1)
      stop("currently, variance decomposition is only available for one variable")
    year_val <- unique(year)
    #initilize output
    output <- matrix(NA, nrow = length(year_val), ncol = 5,
                     dimnames = list(as.character(year_val), NULL))
    i <- 1
    for(y in year_val){
      ind <- year == y
      output[i,] <- var_decomp(y[ind], X[ind, ], wgt[ind])
      i <- i + 1
    }
    
  }
}