#combine lin_skew_dec here

#' Variance and Skewness Decomposition
#'
#'
#' @param y an outcome vector (must be numeric without NA's).
#' @param X matrix or data frame containing variables for the decomposition. Usually, if decomposition by
#'          more than one variable is desired, X should be the output of \code{\link{linear_projection}}.
#' @param wgt an optional vector of weights.
#' @param moment the moment on which the decomposition method is applied.
#'               either "variance" (second moment) or "skewness" (third moment).
#' @param year an optional vector of years. if provided, the decomposition is calculated by year.
#'             otherwise, the decomposition is calculated for the whole sample.  

vs_decomp <- function(y,
                      X,
                      wgt = rep(1, length(y)),
                      moment = "skewness",
                      year = rep(1, length(y))){
  #input validation
  p <- ncol(X)
  year_val <- unique(year)
  num_year <- length(year_val)
  N <- rep(NA, num_year)
  names(N) <- ifelse(year_val == 1, " ", as.character(year_val))
  if(moment == "skewness"){
    if(p == 1){
      num_comp <- 3
      comp_nms <- c("between", "within", "cov")
      comp_se_nms <- c("between_sd", "within_sd", "cov_sd")
      dec_func <- "skew_decomp"
    }
  } else {
    if(p > 1)
      stop("currently, variance decomposition is only available for one variable")
    num_comp <- 2
    comp_nms <- c("between", "within")
    comp_se_nms <- c("between_sd", "within_sd")
    dec_func <- "var_decomp"
  }
  #initilize output
  components <- matrix(NA, nrow = num_year, ncol = num_comp,
                       dimnames = list(as.character(year_val), comp_nms))
  components_se <- matrix(NA, nrow = num_year, ncol = num_comp,
                          dimnames = list(as.character(year_val), comp_se_nms))
  i <- 1
  for(v in year_val){
    ind <- year == v
    tmp <- do.call(dec_func, list(y = y[ind], x = X[ind, ], wgt = wgt[ind]))
    components[i,] <- tmp[1:num_comp]
    components_se[i,] <- tmp[(num_comp + 1):(2*num_comp)]
    N[i] <- sum(ind)
    i <- i + 1
  }
  output <- list(moment = moment,
                 components = components,
                 components_se = components_se,
                 N = N)
  class(output) <- "vs_decomp"
  return(output)
}