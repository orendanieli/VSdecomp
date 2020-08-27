#replicate oren's results

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
#' 
#' @export

vs_decomp <- function(y = NULL,
                      X,
                      wgt = rep(1, nrow(X)),
                      moment = "skewness",
                      year = rep(1, nrow(X))){
  validate_input(y, X, wgt, year)
  #replace "moment" with its abbreviation (var / ske)
  moment <- substr(moment, 1 ,3)
  p <- ncol(X)
  year_val <- unique(year)
  if(is.numeric(year_val)){
    year_val <- year_val[order(year_val)]
  }
  num_year <- length(year_val)
  N <- rep(NA, num_year) #num. of observations in each year
  names(N) <- ifelse(year_val == 1, " ", as.character(year_val))
  #initialize output
  components <- NULL
  components_se <- NULL
  if (p <= 1){
    dec_func <- ifelse(moment == "var", "var_decomp", "skew_decomp")
    num_comp <- ifelse(moment == "var", 2, 3)
    i <- 1
    for(v in year_val){
      ind <- year == v
      tmp <- do.call(dec_func, list(y = y[ind], x = X[ind, ], wgt = wgt[ind]))
      components <- rbind(components, tmp[1:num_comp])
      components_se <- rbind(components_se, tmp[(num_comp + 1):(2*num_comp)])
      N[i] <- sum(ind)
      i <- i + 1
    }
    row.names(components_se) <- names(N)
    type <- "one variable"
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
    type <- "linear"
  }
  row.names(components) <- names(N)
  output <- list(moment = ifelse(moment == "var", "variance", "skewness"),
                 components = components,
                 components_se = components_se,
                 N = N,
                 type = type)
  class(output) <- "vs_decomp"
  return(output)
}

validate_input <- function(y, X, wgt, year){
  n <- nrow(X)
  p <- ncol(X)
  if(is.null(p)){
    stop("X should be a matrix or data frame")
  }
  if(!inherits(wgt, "numeric") | n != length(wgt) | any(is.na(wgt))){
    stop(paste("wgt must be numeric,",
               "with the same number of examples as X.",
               "missing values aren't allowed"))
  }
  #we dont allow for weight = 0 since wtd.mean(1, 0) = NaN
  if(any(wgt <= 0)){
    stop("wgt must be positive.")
  }
  if(n != length(year) | any(is.na(year))){
    stop(paste("year must contain",
               "the same number of examples as X.",
               "missing values aren't allowed"))
  }
  if(p > 1){
    colmeans <- apply(X, 2, wtd.mean, weights = wgt)
    if(!all(colmeans^2 < 0.01)){
      stop(paste("not all columns of X are zero mean.",
                 "please use linear_projection()"))
    }
  }
}