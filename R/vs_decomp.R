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
    components <- matrix(NA, nrow = length(year_val), ncol = 2,
                     dimnames = list(as.character(year_val), c("between", "within")))
    components_sd <- matrix(NA, nrow = length(year_val), ncol = 2,
                     dimnames = list(as.character(year_val), c("between_sd", "within_sd")))
    i <- 1
    for(v in year_val){
      ind <- year == v
      tmp <- var_decomp(y[ind], X[ind, ], wgt[ind])
      components[i,] <- tmp[1:2]
      components_sd[i,] <- tmp[3:4]
      i <- i + 1
    }
    output <- list(moment = moment,
                   components = components,
                   components_sd = components_sd)
    class(output) <- "var_decomp"
  }
  return(output)
}