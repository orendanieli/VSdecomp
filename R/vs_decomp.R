#check whether the code could be more efficient for p = 1.
#write some validation functions for the input.
#plot and summary for p > 1
#play and test p > 1 to see that it actually works (maybe just by replicating oren's results)

#' Skewness and Variance Decomposition
#'
#' decompose the skewness or the variance of an outcome vector into independent components, 
#' either using one or many variables.
#' 
#' @param y an outcome vector (necessary only when decomposing by one variable).
#' @param X when decomposing by one variable, a matrix or data frame containing the variable 
#'          for the decomposition. when decomposing by more than one variable, a matrix or 
#'          data frame containing the linear components of y, calculated by
#'           \code{\link{linear_projection}}.
#' @param wgt an optional vector of weights.
#' @param moment the moment on which the decomposition method is applied.
#'               either "variance" (second moment) or "skewness" (third moment).
#' @param year an optional vector of years. if provided, the decomposition is calculated by year.
#'             otherwise, the decomposition is calculated for the whole sample.  
#' @return 

vs_decomp <- function(y = NULL,
                      X,
                      wgt = rep(1, nrow(X)),
                      moment = "skewness",
                      year = rep(1, nrow(X))){
  #input validation
  #replace "moment" with its abbreviation (var / ske)
  moment <- substr(moment, 1 ,3)
  p <- ncol(X)
  year_val <- unique(year)
  num_year <- length(year_val)
  N <- rep(NA, num_year) #num. of observations in each year
  names(N) <- ifelse(year_val == 1, " ", as.character(year_val))
  if (p <= 1){
    if(moment == "ske"){
      num_comp <- 3
      comp_nms <- c("between", "within", "cov")
      comp_se_nms <- c("between_sd", "within_sd", "cov_sd")
      dec_func <- "skew_decomp"
    } else {
      num_comp <- 2
      comp_nms <- c("between", "within")
      comp_se_nms <- c("between_sd", "within_sd")
      dec_func <- "var_decomp"
    }
    #initilize output
    components <- matrix(NA, nrow = num_year, ncol = num_comp,
                         dimnames = list(names(N), comp_nms))
    components_se <- matrix(NA, nrow = num_year, ncol = num_comp,
                            dimnames = list(names(N), comp_se_nms))
    i <- 1
    for(v in year_val){
      ind <- year == v
      tmp <- do.call(dec_func, list(y = y[ind], x = X[ind, ], wgt = wgt[ind]))
      components[i,] <- tmp[1:num_comp]
      components_se[i,] <- tmp[(num_comp + 1):(2*num_comp)]
      N[i] <- sum(ind)
      i <- i + 1
    }
  } else {
    if(moment == "var")
      stop("currently, variance decomposition is only available for one variable")
    components <- NULL
    i <- 1
    for(v in year_val){
      ind <- year == v
      components <- rbind(components, linear_skew_decomp(X = X[ind, ], wgt = wgt[ind]))
      N[i] <- sum(ind)
      i <- i + 1
    }
    row.names(components) <- names(N)
    components_se <- NULL
  }
  output <- list(moment = ifelse(moment == "var", "variance", "skewness"),
                 components = components,
                 components_se = components_se,
                 N = N)
  class(output) <- "vs_decomp"
  return(output)
}